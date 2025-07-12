{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- Copyright 2020 United States Government as represented by the Administrator
-- of the National Aeronautics and Space Administration. All Rights Reserved.
--
-- Disclaimers
--
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY
-- OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT
-- LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO
-- SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
-- PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE
-- SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF
-- PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN
-- ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR
-- RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR
-- ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE. FURTHER,
-- GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING
-- THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES
-- IT "AS IS."
--
-- Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST
-- THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS
-- ANY PRIOR RECIPIENT. IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN
-- ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE,
-- INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S
-- USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE
-- UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY
-- PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW. RECIPIENT'S SOLE REMEDY
-- FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS
-- AGREEMENT.
--
-- | Create <https://cfs.gsfc.nasa.gov/ NASA Core Flight System> (CFS)
-- applications that subscribe to the communication bus and call Copilot when
-- new messages arrive.
--
-- The applications are created ready to be extracted in the application
-- directory in CFS, and they subscribe to a generic monitor. It is the user's
-- responsibility to modify the generated Copilot and C code to deal with the
-- monitors they'd like to implement, and the data they must manipulate.

{- HLINT ignore "Functor law" -}
module Command.CFSApp
    ( command
    , CommandOptions(..)
    , ErrorCode
    )
  where

-- External imports
import           Control.Applicative    ( liftA2, (<|>) )
import qualified Control.Exception      as E
import           Control.Monad.Except   ( ExceptT (..), liftEither )
import           Data.Aeson             ( ToJSON (..) )
import           Data.Maybe             ( fromMaybe, mapMaybe, maybeToList )
import           GHC.Generics           ( Generic )

-- External imports: auxiliary
import qualified Command.Standalone

-- Internal imports: auxiliary
import Command.Result         ( Result (..) )
import Data.List.Extra        ( stripSuffix )
import Data.String.Extra      ( pascalCase )
import System.Directory.Extra ( copyTemplate )

-- Internal imports
import Command.Common
import Command.Errors     (ErrorCode, ErrorTriplet (..))
import Command.VariableDB (Connection (..), TopicDef (..), TypeDef (..),
                           VariableDB, findConnection, findInput, findTopic,
                           findType, findTypeByType)

-- | Generate a new CFS application connected to Copilot.
command :: CommandOptions
        -> IO (Result ErrorCode)
command options = processResult $ do
    -- Obtain template dir
    templateDir <- locateTemplateDir mTemplateDir "copilot-cfs"

    templateVars <- parseTemplateVarsFile templateVarsF

    appData <- command' options functions

    let subst = mergeObjects (toJSON appData) templateVars

    -- Expand template
    ExceptT $ fmap (makeLeftE cannotCopyTemplate) $ E.try $
      copyTemplate templateDir subst targetDir

  where

    targetDir     = commandTargetDir options
    mTemplateDir  = commandTemplateDir options
    functions     = exprPair (commandPropFormat options)
    templateVarsF = commandExtraVars options

command' :: CommandOptions
         -> ExprPair
         -> ExceptT ErrorTriplet IO AppData
command' options (ExprPair exprT) = do
    -- Open files needed to fill in details in the template.
    vs    <- parseVariablesFile varNameFile
    rs    <- parseRequirementsListFile handlersFile
    varDB <- openVarDBFilesWithDefault varDBFile

    specT <- maybe (return Nothing) (\e -> Just <$> parseInputExpr' e) cExpr
    specF <- maybe (return Nothing) (\f -> Just <$> parseInputFile' f) fp

    let spec = specT <|> specF

    liftEither $ checkArguments spec vs rs

    copilotM <- sequenceA $ (\spec' -> processSpec spec' fp cExpr) <$> spec

    let varNames = fromMaybe (specExtractExternalVariables spec) vs
        monitors = maybe
                     (specExtractHandlers spec)
                     (map (\x -> (x, Nothing)))
                     rs

    let appData   = commandLogic varDB varNames monitors' copilotM
        monitors' = mapMaybe (monitorMap varDB) monitors

    return appData

  where

    cExpr          = commandConditionExpr options
    fp             = commandInputFile options
    varNameFile    = commandVariables options
    varDBFile      = maybeToList $ commandVariableDB options
    handlersFile   = commandHandlers options
    formatName     = commandFormat options
    propFormatName = commandPropFormat options
    propVia        = commandPropVia options

    parseInputExpr' e =
      parseInputExpr e propFormatName propVia exprT

    parseInputFile' f =
      parseInputFile f formatName propFormatName propVia exprT

    processSpec spec' expr' fp' =
      Command.Standalone.commandLogic expr' fp' "copilot" [] exprT spec'

-- | Generate a variable substitution map for a cFS application.
commandLogic :: VariableDB
             -> [String]
             -> [Trigger]
             -> Maybe Command.Standalone.AppData
             -> AppData
commandLogic varDB varNames handlers copilotM =
    AppData vars ids infos datas handlers copilotM
  where

    -- This is a Data.List.unzip4
    (vars, ids, infos, datas) = foldr f ([], [], [], []) varNames

    f n o@(oVars, oIds, oInfos, oDatas) =
      case variableMap varDB n of
        Nothing -> o
        Just (vars, ids, infos, datas) ->
          (vars : oVars, ids : oIds, infos : oInfos, datas : oDatas)

-- ** Argument processing

-- | Options used to customize the conversion of specifications to ROS
-- applications.
data CommandOptions = CommandOptions
  { commandConditionExpr :: Maybe String   -- ^ Trigger condition.
  , commandInputFile   :: Maybe FilePath -- ^ Input specification file.
  , commandTargetDir   :: FilePath       -- ^ Target directory where the
                                         -- application should be created.
  , commandTemplateDir :: Maybe FilePath -- ^ Directory where the template is
                                         -- to be found.
  , commandVariables   :: Maybe FilePath -- ^ File containing a list of
                                         -- variables to make available to
                                         -- Copilot.
  , commandVariableDB  :: Maybe FilePath -- ^ File containing a list of known
                                         -- variables with their types and the
                                         -- message IDs they can be obtained
                                         -- from.
  , commandHandlers    :: Maybe FilePath -- ^ File containing a list of
                                         -- handlers used in the Copilot
                                         -- specification. The handlers are
                                         -- assumed to receive no arguments.
  , commandFormat      :: String         -- ^ Format of the input file.
  , commandPropFormat  :: String         -- ^ Format used for input properties.
  , commandPropVia     :: Maybe String   -- ^ Use external command to
                                         -- pre-process system properties.
  , commandExtraVars   :: Maybe FilePath -- ^ File containing additional
                                         -- variables to make available to the
                                         -- template.
  }

-- | Return the variable information needed to generate declarations
-- and subscriptions for a given variable name and variable database.
variableMap :: VariableDB
            -> String
            -> Maybe (VarDecl, MsgInfoId, MsgInfo, MsgData)
variableMap varDB varName = do
    inputDef  <- findInput varDB varName
    mid       <- connectionTopic <$> findConnection inputDef "cfs"
    topicDef  <- findTopic varDB "cfs" mid
    let typeVar' = fromMaybe
                     (topicType topicDef)
                     (typeToType <$> findType varDB varName "cfs" "C")

    -- Pick name for the function to process a message ID.
    let mn = pascalCase $ stripSuffix "_MID" mid

    return ( VarDecl varName typeVar'
           , mid
           , MsgInfo mid mn
           , MsgData mn varName typeVar'
           )
  where

-- | Return the monitor information needed to generate declarations and
-- publishers for the given monitor info, and variable database.
monitorMap :: VariableDB
           -> (String, Maybe String)
           -> Maybe Trigger
monitorMap varDB (monitorName, Nothing) =
  Just $ Trigger monitorName Nothing Nothing
monitorMap varDB (monitorName, Just ty) = do
  let tyCFS = typeFromType <$> findTypeByType varDB "cfs" "C" ty
  return $ Trigger monitorName (Just ty) tyCFS

-- | The declaration of a variable in C, with a given type and name.
data VarDecl = VarDecl
    { varDeclName :: String
    , varDeclType :: String
    }
  deriving (Generic)

instance ToJSON VarDecl

-- | The message ID to subscribe to.
type MsgInfoId = String

-- | A message ID to subscribe to and the name associated to it. The name is
-- used to generate a suitable name for the message handler.
data MsgInfo = MsgInfo
    { msgInfoId   :: MsgInfoId
    , msgInfoDesc :: String
    }
  deriving (Generic)

instance ToJSON MsgInfo

-- | Information on the data provided by a message with a given description,
-- and the type of the data it carries.
data MsgData = MsgData
    { msgDataDesc    :: String
    , msgDataVarName :: String
    , msgDataVarType :: String
    }
  deriving (Generic)

instance ToJSON MsgData

-- | The message ID to subscribe to.
data Trigger = Trigger
    { triggerName    :: String
    , triggerType    :: Maybe String
    , triggerMsgType :: Maybe String
    }
  deriving (Generic)

instance ToJSON Trigger

-- | Data that may be relevant to generate a cFS monitoring application.
data AppData = AppData
  { variables   :: [VarDecl]
  , msgIds      :: [MsgInfoId]
  , msgCases    :: [MsgInfo]
  , msgHandlers :: [MsgData]
  , triggers    :: [Trigger]
  , copilot     :: Maybe Command.Standalone.AppData
  }
  deriving (Generic)

instance ToJSON AppData

