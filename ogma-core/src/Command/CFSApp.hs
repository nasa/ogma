{-# LANGUAGE OverloadedStrings #-}
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
import qualified Control.Exception         as E
import           Control.Monad             (filterM, forM_)
import           Data.Aeson                (Value (..), decode, object, (.=))
import qualified Data.ByteString.Lazy      as B
import           Data.List                 (find)
import           Data.Text                 (Text)
import           Data.Text.Lazy            (pack, unpack)
import           Data.Text.Lazy.Encoding   (encodeUtf8)
import           Distribution.Simple.Utils (getDirectoryContentsRecursive)
import           System.Directory          (createDirectoryIfMissing,
                                            doesFileExist)
import           System.FilePath           (makeRelative, splitFileName, (</>))
import           Text.Microstache          (compileMustacheFile,
                                            compileMustacheText, renderMustache)

-- Internal imports: auxiliary
import Command.Result ( Result (..) )
import Data.Location  ( Location (..) )

-- Internal imports
import Paths_ogma_core ( getDataDir )

-- | Generate a new CFS application connected to Copilot.
cFSApp :: FilePath       -- ^ Target directory where the application
                         --   should be created.
       -> FilePath       -- ^ File containing a list of variables to make
                         --   available to Copilot.
       -> Maybe FilePath -- ^ File containing a list of known variables
                         --   with their types and the message IDs they
                         --   can be obtained from.
       -> IO (Result ErrorCode)
cFSApp targetDir varNameFile varDBFile = do

  -- We first try to open the two files we need to fill in details in the CFS
  -- app template.
  --
  -- The variable DB is optional, so this check only fails if the filename
  -- provided does not exist or if the file cannot be opened or parsed (wrong
  -- format).
  varDBE <- E.try $
                case varDBFile of
                  Nothing -> return knownVars
                  Just fn -> fmap read <$> lines <$> readFile fn

  case varDBE of
    Left  e     -> return $ cannotOpenDB varDBFile e
    Right varDB -> do

      -- The variable list is mandatory. This check fails if the filename
      -- provided does not exist or if the file cannot be opened. The condition
      -- on the result also checks that the list of variables in the file is
      -- not empty (otherwise, we do not know when to call Copilot).
      varNamesE <- E.try $ lines <$> readFile varNameFile

      case varNamesE of
        Left e         -> return $ cannotOpenVarFile varNameFile e
        Right []       -> return $ cannotEmptyVarList varNameFile
        Right varNames -> do

          -- Obtain template dir
          dataDir <- getDataDir
          let templateDir = dataDir </> "templates" </> "copilot-cfs"

          E.handle (return . cannotCopyTemplate) $ do

            let f n o@(oVars, oIds, oInfos, oDatas) =
                  case variableMap varDB n of
                    Nothing -> o
                    Just (vars, ids, infos, datas) ->
                      (vars : oVars, ids : oIds, infos : oInfos, datas : oDatas)

            -- This is a Data.List.unzip4
            let (vars, ids, infos, datas) = foldr f ([], [], [], []) varNames

            let (variablesS, msgSubscriptionS, msgCasesS, msgHandlerS) =
                  appComponents vars ids infos datas
                subst = object $
                          [ "variablesS"        .= pack variablesS
                          , "msgSubscriptionsS" .= pack msgSubscriptionS
                          , "msgCasesS"         .= pack msgCasesS
                          , "msgHandlerS"       .= pack msgHandlerS
                          ]

            -- Expand template
            copyTemplate templateDir subst targetDir

            return Success

-- | Predefined list of Icarous variables that are known to Ogma
knownVars :: [(String, String, String, String)]
knownVars =
  [ ("position", "position_t", "ICAROUS_POSITION_MID", "IcarousPosition") ]

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

-- | The message ID to subscribe to.
type MsgInfoId = String

-- | A message ID to subscribe to and the name associated to it. The name is
-- used to generate a suitable name for the message handler.
data MsgInfo = MsgInfo
  { msgInfoId   :: MsgInfoId
  , msgInfoDesc :: String
  }

-- | Information on the data provided by a message with a given description,
-- and the type of the data it carries.
data MsgData = MsgData
  { msgDataDesc    :: String
  , msgDataVarName :: String
  , msgDataVarType :: String
  }

-- | Return the components that are customized in a cFS application.
appComponents :: [VarDecl] -> [MsgInfoId] -> [MsgInfo] -> [MsgData]
              -> (String, String, String, String)
appComponents variables msgIds msgNames msgDatas = cfsFileContents
  where
    variablesS = unlines $ map toVarDecl variables
    toVarDecl varDecl = varDeclType varDecl ++ " " ++ varDeclName varDecl ++ ";"

    msgSubscriptionS     = unlines $ map toMsgSubscription msgIds
    toMsgSubscription nm =
      "    CFE_SB_Subscribe(" ++ nm ++ ", COPILOT_CommandPipe);"

    msgCasesS = unlines $ map toMsgCase msgNames
    toMsgCase msgInfo = unlines
      [ "        case " ++ msgInfoId msgInfo ++ ":"
      , "            COPILOT_Process" ++ msgInfoDesc msgInfo ++ "();"
      , "            break;"
      ]

    msgHandlerS = unlines $ map toMsgHandler msgDatas
    toMsgHandler msgData =
      unlines [ "/**"
              , "* Make ICAROUS data available to Copilot and run monitors."
              , "*/"
              , "void COPILOT_Process" ++ msgDataDesc msgData ++ "(void)"
              , "{"
              , "    " ++ msgDataVarType msgData ++ "* msg;"
              , "    msg = (" ++ msgDataVarType msgData ++ "*) COPILOTMsgPtr;"
              , "    " ++ msgDataVarName msgData ++ " = *msg;"
              , ""
              , "    // Run all copilot monitors."
              , "    step();"
              , "}"
              ]

    cfsFileContents =
      ( variablesS
      , msgSubscriptionS
      , msgCasesS
      , msgHandlerS
      )

-- * Exception handlers

-- | Exception handler to deal with the case in which the variable DB cannot be
-- opened.
cannotOpenDB :: Maybe FilePath -> E.SomeException -> Result ErrorCode
cannotOpenDB Nothing _e =
    Error ecCannotOpenDBCritical msg LocationNothing
  where
    msg =
      "cannotOpenDB: this is a bug. Please notify the developers"
cannotOpenDB (Just file) _e =
    Error ecCannotOpenDBUser msg (LocationFile file)
  where
    msg =
      "cannot open variable DB file " ++ file

-- | Exception handler to deal with the case in which the variable file
-- provided by the user cannot be opened.
cannotOpenVarFile :: FilePath -> E.SomeException -> Result ErrorCode
cannotOpenVarFile file _e =
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

-- | Exception handler to deal with the case of files that cannot be
-- copied/generated due lack of space or permissions or some I/O error.
cannotCopyTemplate :: E.SomeException -> Result ErrorCode
cannotCopyTemplate _e =
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

-- | Internal error: Variable DB cannot be opened.
ecCannotOpenDBCritical :: ErrorCode
ecCannotOpenDBCritical = 2

-- | Error: the variable DB provided by the user cannot be opened.
ecCannotOpenDBUser :: ErrorCode
ecCannotOpenDBUser = 1

-- | Error: the variable file provided by the user cannot be opened.
ecCannotOpenVarFile :: ErrorCode
ecCannotOpenVarFile = 1

-- | Error: the variable file provided contains an empty list.
ecCannotEmptyVarList :: ErrorCode
ecCannotEmptyVarList = 1

-- | Error: the files cannot be copied/generated due lack of space or
-- permissions or some I/O error.
ecCannotCopyTemplate :: ErrorCode
ecCannotCopyTemplate = 1

-- * Generic template handling

-- | Copy a template directory into a target location, expanding variables
-- provided in a map in a JSON value, both in the file contents and in the
-- filepaths themselves.
copyTemplate :: FilePath -> Value -> FilePath -> IO ()
copyTemplate templateDir subst targetDir = do

  -- Get all files (not directories) in the template dir. To keep a directory,
  -- create an empty file in it (e.g., .keep).
  tmplContents <- map (templateDir </>) . filter (`notElem` ["..", "."])
                    <$> getDirectoryContentsRecursive templateDir
  tmplFiles <- filterM doesFileExist tmplContents

  -- Copy files to new locations, expanding their name and contents as
  -- mustache templates.
  forM_ tmplFiles $ \fp -> do

    -- New file name in target directory, treating file
    -- name as mustache template.
    let fullPath = targetDir </> newFP
          where
            -- If file name has mustache markers, expand, otherwise use
            -- relative file path
            newFP = either (const relFP)
                           (unpack . (`renderMustache` subst))
                           fpAsTemplateE

            -- Local file name within template dir
            relFP = makeRelative templateDir fp

            -- Apply mustache substitutions to file name
            fpAsTemplateE = compileMustacheText "fp" (pack relFP)

    -- File contents, treated as a mustache template.
    contents <- encodeUtf8 <$> (`renderMustache` subst)
                           <$> compileMustacheFile fp

    -- Create target directory if necessary
    let dirName = fst $ splitFileName fullPath
    createDirectoryIfMissing True dirName

    -- Write expanded contents to expanded file path
    B.writeFile fullPath contents
