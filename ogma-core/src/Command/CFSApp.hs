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
    ( cFSApp
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
import           Data.Functor           ( (<&>) )
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
cFSApp :: FilePath       -- ^ Target directory where the application
                         --   should be created.
       -> Maybe FilePath -- ^ Directory where the template is to be found.
       -> FilePath       -- ^ File containing a list of variables to make
                         --   available to Copilot.
       -> Maybe FilePath -- ^ File containing a list of known variables
                         --   with their types and the message IDs they
                         --   can be obtained from.
       -> Maybe FilePath -- ^ File containing a list of handlers used in the
			 --   Copilot specification. The handlers are assumed
                         --   to receive no arguments.
       -> Maybe FilePath -- ^ File containing additional variables to make
                         --   available to the template.
       -> IO (Result ErrorCode)
cFSApp targetDir mTemplateDir varNameFile varDBFile handlersFile
       templateVarsF = exceptToResult $ do

  -- We first try to open the two files we need to fill in details in the CFS
  -- app template.
  --
  -- The variable DB is optional, so this check only fails if the filename
  -- provided does not exist or if the file cannot be opened or parsed (wrong
  -- format).
  varDB <- parseVarDBFile varDBFile

  handlers <- parseOptionalRequirementsListFile handlersFile

  templateVars <- parseOptionalTemplateVarsFile templateVarsF

  varNames <- parseVarNames varNameFile

  templateDir <- locateTemplateDir mTemplateDir "copilot-cfs"

  let subst = cfsAppLogic varDB varNames templateVars handlers

  -- Expand template
  ExceptT $ fmap (makeLeftE CannotCopyTemplate) $ E.try $
    copyTemplate templateDir subst targetDir

-- | Generate a variable substitution map for a cFS application.
cfsAppLogic :: [(String, String, String, String)]
            -> [String]
            -> Value
            -> [Trigger]
            -> Value
cfsAppLogic varDB varNames templateVars handlers =
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

-- | Parse a file containing a list of cFS variables and how they associate to
-- different cFS bus messages and types.
parseVarDBFile :: Maybe FilePath
               -> ExceptT ErrorReasons IO [(String, String, String, String)]
parseVarDBFile Nothing   = pure []
parseVarDBFile (Just fn) =
  ExceptT $ makeLeftE (CannotOpenDB fn) <$>
    (E.try $ fmap read <$> lines <$> readFile fn)

-- | Parse the file containing the list of variables to monitor.
--
-- The variable list is mandatory. This check fails if the filename provided
-- does not exist or if the file cannot be opened. The condition on the result
-- also checks that the list of variables in the file is not empty (otherwise,
-- we do not know when to call Copilot).
parseVarNames :: FilePath
              -> ExceptT ErrorReasons IO [String]
parseVarNames varNameFile = ExceptT $ do
  varNamesE <- E.try $ lines <$> readFile varNameFile
  case (varNamesE :: Either E.SomeException [String]) of
    Left _         -> return $ Left $ CannotOpenVarFile varNameFile
    Right []       -> return $ Left $ CannotEmptyVarList varNameFile
    Right varNames -> return $ Right varNames

-- | Process a requirements / handlers list file, if available, and return the
-- handler names.
parseOptionalRequirementsListFile :: Maybe FilePath
                                  -> ExceptT ErrorReasons IO [String]
parseOptionalRequirementsListFile Nothing   = return []
parseOptionalRequirementsListFile (Just fp) =
  ExceptT $ makeLeftE (CannotOpenHandlers fp) <$>
    (E.try $ lines <$> readFile fp)

-- | Process a JSON file with additional template variables to make available
-- during template expansion.
parseOptionalTemplateVarsFile :: Maybe FilePath
                              -> ExceptT ErrorReasons IO Value
parseOptionalTemplateVarsFile Nothing   = return $ object []
parseOptionalTemplateVarsFile (Just fp) = do
  content <- liftIO $ B.safeReadFile fp
  let value = eitherDecode =<< content
  case value of
    Right x@(Object _) -> return x
    Right x@Null       -> return x
    Right _            -> throwError (CannotReadObjectTemplateVars fp)
    _                  -> throwError (CannotOpenTemplateVars fp)

-- * Error handlers

-- | List of reasons why generating a cFS application may fail.
data ErrorReasons = CannotOpenDB FilePath
                  | CannotOpenVarFile FilePath
                  | CannotEmptyVarList FilePath
                  | CannotOpenHandlers FilePath
                  | CannotOpenTemplateVars FilePath
                  | CannotReadObjectTemplateVars FilePath
                  | CannotCopyTemplate

-- | Map a result in an @ExceptT@ monad transformer into a top-level
-- application result.
exceptToResult :: ExceptT ErrorReasons IO () -> IO (Result ErrorCode)
exceptToResult e = runExceptT e <&> \res ->
  case res of
    Right ()                               -> Success
    Left (CannotOpenDB fp)                 -> cannotOpenDB fp
    Left (CannotOpenVarFile fp)            -> cannotOpenVarFile fp
    Left (CannotEmptyVarList fp)           -> cannotEmptyVarList fp
    Left (CannotOpenHandlers fp)           -> cannotOpenHandlers fp
    Left (CannotOpenTemplateVars fp)       -> cannotOpenTemplateVars fp
    Left (CannotReadObjectTemplateVars fp) -> cannotReadObjectTemplateVars fp
    Left CannotCopyTemplate                -> cannotCopyTemplate

-- | Exception handler to deal with the case in which the variable DB cannot be
-- opened.
cannotOpenDB :: FilePath -> Result ErrorCode
cannotOpenDB file =
    Error ecCannotOpenDBUser msg (LocationFile file)
  where
    msg =
      "cannot open variable DB file " ++ file

-- | Exception handler to deal with the case in which the variable file
-- provided by the user cannot be opened.
cannotOpenVarFile :: FilePath -> Result ErrorCode
cannotOpenVarFile file =
    Error ecCannotOpenVarFile  msg (LocationFile file)
  where
    msg =
      "cannot open variable list file " ++ file

-- | Exception handler to deal with the case of the variable file provided
-- containing an empty list.
cannotEmptyVarList :: FilePath -> Result ErrorCode
cannotEmptyVarList file =
    Error ecCannotEmptyVarList msg (LocationFile file)
  where
    msg =
      "variable list in file " ++ file ++ " is empty"

-- | Exception handler to deal with the case in which the handlers file cannot
-- be opened.
cannotOpenHandlers :: FilePath -> Result ErrorCode
cannotOpenHandlers file =
    Error ecCannotOpenHandlersUser msg (LocationFile file)
  where
    msg =
      "cannot open handlers file " ++ file

-- | Exception handler to deal with the case in which the template vars file
-- cannot be opened.
cannotOpenTemplateVars :: FilePath -> Result ErrorCode
cannotOpenTemplateVars file =
    Error ecCannotOpenTemplateVarsUser msg (LocationFile file)
  where
    msg =
      "Cannot open file with additional template variables: " ++ file

-- | Exception handler to deal with the case in which the template vars file
-- cannot be opened.
cannotReadObjectTemplateVars :: FilePath -> Result ErrorCode
cannotReadObjectTemplateVars file =
    Error ecCannotReadObjectTemplateVarsUser msg (LocationFile file)
  where
    msg =
      "Cannot open file with additional template variables: " ++ file

-- | Exception handler to deal with the case of files that cannot be
-- copied/generated due lack of space or permissions or some I/O error.
cannotCopyTemplate :: Result ErrorCode
cannotCopyTemplate =
    Error ecCannotCopyTemplate msg LocationNothing
  where
    msg =
      "CFS app generation failed during copy/write operation. Check that"
      ++ " there's free space in the disk and that you have the necessary"
      ++ " permissions to write in the destination directory."

-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error codes used are 1 for user error, and 2 for internal bug.
type ErrorCode = Int

-- | Error: the variable DB provided by the user cannot be opened.
ecCannotOpenDBUser :: ErrorCode
ecCannotOpenDBUser = 1

-- | Error: the variable file provided by the user cannot be opened.
ecCannotOpenVarFile :: ErrorCode
ecCannotOpenVarFile = 1

-- | Error: the variable file provided contains an empty list.
ecCannotEmptyVarList :: ErrorCode
ecCannotEmptyVarList = 1

-- | Error: the handlers file provided by the user cannot be opened.
ecCannotOpenHandlersUser :: ErrorCode
ecCannotOpenHandlersUser = 1

-- | Error: the template vars file provided by the user cannot be opened.
ecCannotOpenTemplateVarsUser :: ErrorCode
ecCannotOpenTemplateVarsUser = 1

-- | Error: the template variables file passed does not contain a JSON object.
ecCannotReadObjectTemplateVarsUser :: ErrorCode
ecCannotReadObjectTemplateVarsUser = 1

-- | Error: the files cannot be copied/generated due lack of space or
-- permissions or some I/O error.
ecCannotCopyTemplate :: ErrorCode
ecCannotCopyTemplate = 1

-- * Auxiliary Functions

-- | Return the path to the template directory.
locateTemplateDir :: Maybe FilePath
                  -> FilePath
                  -> ExceptT ErrorReasons IO FilePath
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
