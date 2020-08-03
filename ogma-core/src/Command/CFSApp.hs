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
import qualified Control.Exception as E
import           Data.List         ( find )
import           System.FilePath   ( (</>) )

-- External imports: auxiliary
import System.Directory.Extra ( copyDirectoryRecursive )

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
            -- Expand template
            copyDirectoryRecursive templateDir targetDir

            let f n o@(oVars, oIds, oInfos, oDatas) =
                  case variableMap varDB n of
                    Nothing -> o
                    Just (vars, ids, infos, datas) ->
                      (vars : oVars, ids : oIds, infos : oInfos, datas : oDatas)

            -- This is a Data.List.unzip4
            let (vars, ids, infos, datas) = foldr f ([], [], [], []) varNames

            let cfsFileName = targetDir </> "fsw" </> "src" </> "copilot_cfs.c"
                cfsFileContents = unlines $ fileContents vars ids infos datas

            writeFile cfsFileName cfsFileContents
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
  { varDeclName    :: String
  , varDeclType    :: String
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
  { msgDataDesc :: String
  , msgDataVarName :: String
  , msgDataVarType :: String
  }

-- | Return the contents of the main CFS application.
fileContents :: [VarDecl] -> [MsgInfoId] -> [MsgInfo] -> [MsgData] -> [String]
fileContents variables msgIds msgNames msgDatas = cfsFileContents
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
      [ "/********************************************************************"
      , "** File: copilot_cfs.c"
      , "**"
      , "** Purpose:"
      , "**   This file contains the source code for the Sample App."
      , "**"
      , "*********************************************************************/"
      , ""
      , "/*"
      , "**   Include Files:"
      , "*/"
      , ""
      , "#include \"copilot_cfs.h\""
      , "#include \"copilot_cfs_perfids.h\""
      , "#include \"copilot_cfs_msgids.h\""
      , "#include \"copilot_cfs_msg.h\""
      , "#include \"copilot_cfs_events.h\""
      , "#include \"copilot_cfs_version.h\""
      , "#include \"msgids/ardupilot_msgids.h\""
      , "#include <msgdef/ardupilot_msg.h>"
      , ""
      , variablesS
      , "void split(void);"
      , "void step(void);"
      , ""
      , "/*"
      , "** global data"
      , "*/"
      , ""
      , "copilot_hk_tlm_t    COPILOT_HkTelemetryPkt;"
      , "CFE_SB_PipeId_t    COPILOT_CommandPipe;"
      , "CFE_SB_MsgPtr_t    COPILOTMsgPtr;"
      , ""
      , "static CFE_EVS_BinFilter_t  COPILOT_EventFilters[] ="
      , "       {  /* Event ID    mask */"
      , "          {COPILOT_STARTUP_INF_EID,       0x0000},"
      , "          {COPILOT_COMMAND_ERR_EID,       0x0000},"
      , "          {COPILOT_COMMANDCPVIOL_INF_EID,    0x0000},"
      , "       };"
      , ""
      , "/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */"
      , "/* COPILOT_AppMain() -- App entry point and main process loop  */"
      , "/*                                                             */"
      , "/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */"
      , "void COPILOT_AppMain( void )"
      , "{"
      , "    int32  status;"
      , "    uint32 RunStatus = CFE_ES_APP_RUN;"
      , ""
      , "    CFE_ES_PerfLogEntry(COPILOT_CFS_PERF_ID);"
      , ""
      , "    COPILOT_AppInit();"
      , ""
      , "    /*"
      , "    ** COPILOT Runloop"
      , "    */"
      , "    while (CFE_ES_RunLoop(&RunStatus) == TRUE)"
      , "    {"
      , "        CFE_ES_PerfLogExit(COPILOT_CFS_PERF_ID);"
      , ""
      , "        // Pend on receipt of command packet"
      , "        // (timeout set to 500 millisecs)"
      , "        status = CFE_SB_RcvMsg (&COPILOTMsgPtr,"
      , "                                COPILOT_CommandPipe,"
      , "                                500);"
      , "        "
      , "        CFE_ES_PerfLogEntry(COPILOT_CFS_PERF_ID);"
      , ""
      , "        if (status == CFE_SUCCESS)"
      , "        {"
      , "            COPILOT_ProcessCommandPacket();"
      , "        }"
      , ""
      , "    }"
      , ""
      , "    CFE_ES_ExitApp(RunStatus);"
      , ""
      , "} /* End of COPILOT_AppMain() */"
      , ""
      , "/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */"
      , "/*                                                             */"
      , "/* COPILOT_AppInit() --  initialization                        */"
      , "/*                                                             */"
      , "/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */"
      , "void COPILOT_AppInit(void)"
      , "{"
      , "    // Register the app with Executive services"
      , "    CFE_ES_RegisterApp();"
      , ""
      , "    // Register the events"
      , "    CFE_EVS_Register(COPILOT_EventFilters,"
      , "                     sizeof(COPILOT_EventFilters) / "
        ++ "sizeof(CFE_EVS_BinFilter_t),"
      , "                     CFE_EVS_BINARY_FILTER);"
      , ""
      , "    // Create the Software Bus command pipe and subscribe to "
      , "    // housekeeping messages"
      , "    CFE_SB_CreatePipe(&COPILOT_CommandPipe, COPILOT_PIPE_DEPTH,"
        ++ "\"COPILOT_CMD_PIPE\");"
      , msgSubscriptionS
      , ""
      , "    CFE_EVS_SendEvent (COPILOT_STARTUP_INF_EID,"
      , "                       CFE_EVS_INFORMATION,"
      , "                      \"COPILOT App Initialized. Ver %d.%d.%d.%d\","
      , "                       COPILOT_CFS_MAJOR_VERSION,"
      , "                       COPILOT_CFS_MINOR_VERSION, "
      , "                       COPILOT_CFS_REVISION, "
      , "                       COPILOT_CFS_MISSION_REV);"
      , ""
      , "} /* End of COPILOT_AppInit() */"
      , ""
      , "/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */"
      , "/*  Name:  COPILOT_ProcessCommandPacket                        */"
      , "/*                                                             */"
      , "/*  Purpose:                                                   */"
      , "/*     This routine will process any packet that is received   */"
      , "/*      on the COPILOT command pipe.                           */"
      , "/*                                                             */"
      , "/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */"
      , "void COPILOT_ProcessCommandPacket(void)"
      , "{"
      , "    CFE_SB_MsgId_t  MsgId;"
      , ""
      , "    MsgId = CFE_SB_GetMsgId(COPILOTMsgPtr);"
      , ""
      , "    switch (MsgId)"
      , "    {"
      , msgCasesS
      , "        default:"
      , "            COPILOT_HkTelemetryPkt.copilot_command_error_count++;"
      , "            CFE_EVS_SendEvent(COPILOT_COMMAND_ERR_EID,CFE_EVS_ERROR,"
      , "                              "
        ++ "\"COPILOT: invalid command packet,MID = 0x%x\","
      , "                              MsgId);"
      , "            break;"
      , "    }"
      , ""
      , "    return;"
      , ""
      , "} /* End COPILOT_ProcessCommandPacket */"
      , ""
      , msgHandlerS
      , ""
      , "/**"
      , " * Report copilot property violations."
      , " */"
      , "void split(void) {"
      , "    CFE_EVS_SendEvent(COPILOT_COMMANDCPVIOL_INF_EID, CFE_EVS_ERROR,"
      , "        \"COPILOT: violation\");"
      , "}"
      ]

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
