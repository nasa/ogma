{-# LANGUAGE DeriveGeneric #-}
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
import qualified Control.Exception      as E
import           Control.Monad.Except   ( ExceptT (..), liftEither, runExceptT,
                                          throwError )
import           Control.Monad.IO.Class ( liftIO )
import           Data.Aeson             ( ToJSON (..), Value (Null, Object),
                                          decode, eitherDecode, object )
import           Data.Aeson.KeyMap      ( union )
import           Data.List              ( find )
import           Data.Text              ( Text )
import           Data.Text.Lazy         ( unpack )
import           GHC.Generics           ( Generic )
import           System.FilePath        ( (</>) )

-- Internal imports: auxiliary
import Command.Result         ( Result (..) )
import Data.ByteString.Extra  as B ( safeReadFile )
import Data.Location          ( Location (..) )
import System.Directory.Extra ( copyTemplate )

-- Internal imports
import Paths_ogma_core ( getDataDir )

-- | Generate a new CFS application connected to Copilot.
command :: CommandOptions
        -> IO (Result ErrorCode)
command options = processResult $ do
    -- Open files needed to fill in details in the template.
    varDB <- parseVarDBFile varDBFile

    handlers <- parseRequirementsListFile handlersFile

    templateVars <- parseTemplateVarsFile templateVarsF

    varNames <- parseVariablesFile varNameFile

    templateDir <- locateTemplateDir mTemplateDir "copilot-cfs"

    let subst = commandLogic varDB varNames templateVars handlers

    -- Expand template
    ExceptT $ fmap (makeLeftE cannotCopyTemplate) $ E.try $
      copyTemplate templateDir subst targetDir

  where

    targetDir     = commandTargetDir options
    mTemplateDir  = commandTemplateDir options
    varNameFile   = commandVariables options
    varDBFile     = commandVariableDB options
    handlersFile  = commandHandlers options
    templateVarsF = commandExtraVars options

-- | Generate a variable substitution map for a cFS application.
commandLogic :: [(String, String, String, String)]
             -> [String]
             -> Value
             -> [Trigger]
             -> Value
commandLogic varDB varNames templateVars handlers =
    mergeObjects subst templateVars
  where
    subst = toJSON $ AppData vars ids infos datas handlers

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
  { commandTargetDir   :: FilePath       -- ^ Target directory where the
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
  , commandExtraVars   :: Maybe FilePath -- ^ File containing additional
                                         -- variables to make available to the
                                         -- template.
  }

-- | Return the variable information needed to generate declarations
-- and subscriptions for a given variable name and variable database.
variableMap :: [(String, String, String, String)]
            -> String
            -> Maybe (VarDecl, MsgInfoId, MsgInfo, MsgData)
variableMap varDB varName =
    csvToVarMap <$> find (sameName varName) varDB

  where

    -- True if the given variable and db entry have the same name
    sameName :: String
             -> (String, String, String, String)
             -> Bool
    sameName n (vn, _, _, _) = n == vn

    -- Convert a DB row into Variable info needed to generate the CFS file
    csvToVarMap :: (String, String, String, String)
                -> (VarDecl, String, MsgInfo, MsgData)
    csvToVarMap (nm, ty, mid, mn) =
      (VarDecl nm ty, mid, MsgInfo mid mn, MsgData mn nm ty)

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
type Trigger = String

-- | Data that may be relevant to generate a cFS monitoring application.
data AppData = AppData
  { variables   :: [VarDecl]
  , msgIds      :: [MsgInfoId]
  , msgCases    :: [MsgInfo]
  , msgHandlers :: [MsgData]
  , triggers    :: [Trigger]
  }
  deriving (Generic)

instance ToJSON AppData

-- | Parse the file containing the list of variables to monitor.
--
-- This check fails if the filename provided does not exist or if the file
-- cannot be opened. The condition on the result also checks that the list of
-- variables in the file is not empty (otherwise, we do not know when to call
-- Copilot).

parseVariablesFile :: Maybe FilePath
                   -> ExceptT ErrorTriplet IO [String]
parseVariablesFile Nothing             = return []
parseVariablesFile (Just fp) = do
  -- Fail if the file cannot be opened.
  varNamesE <- liftIO $ E.try $ lines <$> readFile fp
  case (varNamesE :: Either E.SomeException [String]) of
    Left _         -> throwError $ cannotOpenVarFile fp
    Right varNames -> return varNames

-- | Process a requirements / handlers list file, if available, and return the
-- handler names.
parseRequirementsListFile :: Maybe FilePath
                          -> ExceptT ErrorTriplet IO [String]
parseRequirementsListFile Nothing   = return []
parseRequirementsListFile (Just fp) =
  ExceptT $ makeLeftE (cannotOpenHandlersFile fp) <$>
    (E.try $ lines <$> readFile fp)

-- | Process a variable database file, if available, and return the rows in it.
parseVarDBFile :: Read a
               => Maybe FilePath
               -> ExceptT ErrorTriplet IO [a]
parseVarDBFile Nothing   = return []
parseVarDBFile (Just fn) =
  ExceptT $ makeLeftE (cannotOpenDB fn) <$>
    (E.try $ fmap read <$> lines <$> readFile fn)

-- | Process a JSON file with additional template variables to make available
-- during template expansion.
parseTemplateVarsFile :: Maybe FilePath
                      -> ExceptT ErrorTriplet IO Value
parseTemplateVarsFile Nothing   = return $ object []
parseTemplateVarsFile (Just fp) = do
  content <- liftIO $ B.safeReadFile fp
  let value = eitherDecode =<< content
  case value of
    Right x@(Object _) -> return x
    Right x@Null       -> return x
    Right _            -> throwError (cannotReadObjectTemplateVars fp)
    _                  -> throwError (cannotOpenTemplateVars fp)

-- * Errors

-- | A triplet containing error information.
data ErrorTriplet = ErrorTriplet ErrorCode String Location

-- | Encoding of reasons why the command can fail.
--
-- The error codes used are 1 for user error, and 2 for internal bug.
type ErrorCode = Int

-- | Process a computation that can fail with an error code, and turn it into a
-- computation that returns a 'Result'.
processResult :: Monad m => ExceptT ErrorTriplet m a -> m (Result ErrorCode)
processResult m = do
  r <- runExceptT m
  case r of
    Left (ErrorTriplet errorCode msg location)
      -> return $ Error errorCode msg location
    _ -> return Success

-- | Exception handler to deal with the case in which the variable DB cannot be
-- opened.
cannotOpenDB :: FilePath -> ErrorTriplet
cannotOpenDB file =
    ErrorTriplet ecCannotOpenDBFile msg (LocationFile file)
  where
    msg =
      "cannot open variable DB file " ++ file

-- | Exception handler to deal with the case in which the variable file
-- provided by the user cannot be opened.
cannotOpenVarFile :: FilePath -> ErrorTriplet
cannotOpenVarFile file =
    ErrorTriplet ecCannotOpenVarFile  msg (LocationFile file)
  where
    msg =
      "cannot open variable list file " ++ file

-- | Exception handler to deal with the case in which the handlers file cannot
-- be opened.
cannotOpenHandlersFile :: FilePath -> ErrorTriplet
cannotOpenHandlersFile file =
    ErrorTriplet ecCannotOpenHandlersFile msg (LocationFile file)
  where
    msg =
      "cannot open handlers file " ++ file

-- | Exception handler to deal with the case in which the template vars file
-- cannot be opened.
cannotOpenTemplateVars :: FilePath -> ErrorTriplet
cannotOpenTemplateVars file =
    ErrorTriplet ecCannotOpenTemplateVarsFile msg (LocationFile file)
  where
    msg =
      "Cannot open file with additional template variables: " ++ file

-- | Exception handler to deal with the case in which the template vars file
-- cannot be opened.
cannotReadObjectTemplateVars :: FilePath -> ErrorTriplet
cannotReadObjectTemplateVars file =
    ErrorTriplet ecCannotReadObjectTemplateVarsFile msg (LocationFile file)
  where
    msg =
      "Cannot open file with additional template variables: " ++ file

-- | Exception handler to deal with the case of files that cannot be
-- copied/generated due lack of space or permissions or some I/O error.
cannotCopyTemplate :: ErrorTriplet
cannotCopyTemplate =
    ErrorTriplet ecCannotCopyTemplate msg LocationNothing
  where
    msg =
      "Generation failed during copy/write operation. Check that"
      ++ " there's free space in the disk and that you have the necessary"
      ++ " permissions to write in the destination directory."

-- ** Error codes


-- | Error: the variable DB provided by the user cannot be opened.
ecCannotOpenDBFile :: ErrorCode
ecCannotOpenDBFile = 1

-- | Error: the variable file provided by the user cannot be opened.
ecCannotOpenVarFile :: ErrorCode
ecCannotOpenVarFile = 1

-- | Error: the handlers file provided by the user cannot be opened.
ecCannotOpenHandlersFile :: ErrorCode
ecCannotOpenHandlersFile = 1

-- | Error: the template vars file provided by the user cannot be opened.
ecCannotOpenTemplateVarsFile :: ErrorCode
ecCannotOpenTemplateVarsFile = 1

-- | Error: the template variables file passed does not contain a JSON object.
ecCannotReadObjectTemplateVarsFile :: ErrorCode
ecCannotReadObjectTemplateVarsFile = 1

-- | Error: the files cannot be copied/generated due lack of space or
-- permissions or some I/O error.
ecCannotCopyTemplate :: ErrorCode
ecCannotCopyTemplate = 1

-- * Auxiliary Functions

-- | Return the path to the template directory.
locateTemplateDir :: Maybe FilePath
                  -> FilePath
                  -> ExceptT e IO FilePath
locateTemplateDir mTemplateDir name =
  case mTemplateDir of
    Just x  -> return x
    Nothing -> liftIO $ do
      dataDir <- getDataDir
      return $ dataDir </> "templates" </> name

-- | Merge two JSON objects.
--
-- Fails if the values are not objects or null.
mergeObjects :: Value -> Value -> Value
mergeObjects (Object m1) (Object m2) = Object (union m1 m2)
mergeObjects obj         Null        = obj
mergeObjects Null        obj         = obj
mergeObjects _           _           = error "The values passed are not objects"

-- | Replace the left Exception in an Either.
makeLeftE :: c -> Either E.SomeException b -> Either c b
makeLeftE c (Left _)   = Left c
makeLeftE _ (Right x)  = Right x
