{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- Copyright 2022 United States Government as represented by the Administrator
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
-- | Create <https://www.ros.org/ Robot Operating System> (ROS) applications
-- that subscribe to obtain data and call Copilot when new values arrive.
--
-- It is the user's responsibility to modify the generated Copilot/C/C++ code
-- to deal with the monitors they'd like to implement, and the data they must
-- manipulate.

{- HLINT ignore "Functor law" -}
module Command.ROSApp
    ( command
    , CommandOptions(..)
    , ErrorCode
    )
  where

-- External imports
import           Control.Applicative  (liftA2, (<|>))
import qualified Control.Exception    as E
import           Control.Monad.Except (ExceptT (..), liftEither)
import           Data.Aeson           (ToJSON (..))
import           Data.Maybe           (fromMaybe, mapMaybe, maybeToList)
import           GHC.Generics         (Generic)

-- External imports: auxiliary
import System.Directory.Extra (copyTemplate)

import qualified Command.Standalone

-- Internal imports: auxiliary
import Command.Result (Result (..))

-- Internal imports
import Command.Common
import Command.Errors     (ErrorCode, ErrorTriplet (..))
import Command.VariableDB (Connection (..), InputDef (..), TopicDef (..),
                           TypeDef (..), VariableDB, findConnection, findInput,
                           findTopic, findType, findTypeByType)

-- | Generate a new ROS application connected to Copilot.
command :: CommandOptions -- ^ Options to the ROS backend.
        -> IO (Result ErrorCode)
command options = processResult $ do
    -- Obtain template dir
    templateDir <- locateTemplateDir mTemplateDir "ros"

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

    let appData   = AppData variables monitors' copilotM
        variables = mapMaybe (variableMap varDB) varNames
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
            -> Maybe VarDecl
variableMap varDB varName = do
  inputDef <- findInput varDB varName
  mid      <- connectionTopic <$> findConnection inputDef "ros/message"
  topicDef <- findTopic varDB "ros/message" mid
  typeVar' <- maybe
                (inputType inputDef)
                (Just . typeToType)
                (findType varDB varName "ros/variable" "C")
  let typeMsg' = fromMaybe
                   (topicType topicDef)
                   (typeFromType <$> findType varDB varName "ros/message" "C")
  return $ VarDecl varName typeVar' mid typeMsg'

-- | Return the monitor information needed to generate declarations and
-- publishers for the given monitor info, and variable database.
monitorMap :: VariableDB
           -> (String, Maybe String)
           -> Maybe Monitor
monitorMap varDB (monitorName, Nothing) =
  Just $ Monitor monitorName Nothing Nothing
monitorMap varDB (monitorName, Just ty) = do
  let ty1 = maybe ty typeFromType $ findTypeByType varDB "ros/variable" "C" ty
  ty2 <- typeFromType <$> findTypeByType varDB "ros/message" "C" ty
  return $ Monitor monitorName (Just ty1) (Just ty2)

-- | The declaration of a variable in C, with a given type and name.
data VarDecl = VarDecl
    { varDeclName    :: String
    , varDeclType    :: String
    , varDeclId      :: String
    , varDeclMsgType :: String
    }
  deriving Generic

instance ToJSON VarDecl

-- | The name of a handler associated to each condition, and the type
-- of value it receives, together with the type for the message.
data Monitor = Monitor
    { monitorName    :: String
    , monitorType    :: Maybe String
    , monitorMsgType :: Maybe String
    }
  deriving Generic

instance ToJSON Monitor

-- | Data that may be relevant to generate a ROS application.
data AppData = AppData
  { variables :: [VarDecl]
  , monitors  :: [Monitor]
  , copilot   :: Maybe Command.Standalone.AppData
  }
  deriving (Generic)

instance ToJSON AppData
