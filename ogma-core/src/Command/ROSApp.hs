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
    ( rosApp
    , ErrorCode
    )
  where

-- External imports
import qualified Control.Exception    as E
import           Control.Monad.Except (ExceptT, liftEither, liftIO, runExceptT,
                                       throwError)
import           Data.Aeson           (eitherDecode)
import           Data.List            (find, intersperse)
import           Data.Maybe           (fromMaybe)
import           System.FilePath      ((</>))

-- External imports: auxiliary
import Data.ByteString.Extra  as B (safeReadFile)
import Data.String.Extra      (sanitizeLCIdentifier, sanitizeUCIdentifier)
import System.Directory.Extra (copyDirectoryRecursive)

-- External imports: ogma
import Data.OgmaSpec            (Spec, externalVariableName, externalVariables,
                                 requirementName, requirements)
import Language.JSONSpec.Parser (JSONFormat (..), parseJSONSpec)

-- Internal imports: auxiliary
import Command.Result                 (Result (..))
import Data.Location                  (Location (..))

-- Internal imports
import Paths_ogma_core ( getDataDir )

-- * ROS app generation

-- | Generate a new ROS application connected to Copilot.
rosApp :: FilePath       -- ^ Target directory where the application
                         --   should be created.
       -> Maybe FilePath -- ^ FRET Component specification file.
       -> Maybe FilePath -- ^ File containing a list of variables to make
                         --   available to Copilot.
       -> Maybe FilePath -- ^ File containing a list of known variables
                         --   with their types and the message IDs they
                         --   can be obtained from.
       -> Maybe FilePath -- ^ File containing a list of handlers used in the
                         --   Copilot specification. The handlers are assumed
                         --   to receive no arguments.
       -> [String]       -- ^ Additional applications to turn on during testing
       -> [String]       -- ^ Limited list of variables to use for testing
       -> IO (Result ErrorCode)
rosApp targetDir fretCSFile varNameFile varDBFile handlersFile testingAdditionalApps testingLimitedVars  =
  processResult $ do
    cs    <- parseOptionalFRETCS fretCSFile
    vs    <- parseOptionalVariablesFile varNameFile
    rs    <- parseOptionalRequirementsListFile handlersFile
    varDB <- parseOptionalVarDBFile varDBFile

    liftEither $ checkArguments cs vs rs

    let varNames = fromMaybe (fretCSExtractExternalVariables cs) vs
        monitors = fromMaybe (fretCSExtractHandlers cs) rs

    e <- liftIO $ rosApp' targetDir varNames varDB monitors testingAdditionalApps testingLimitedVars
    liftEither e

-- | Generate a new ROS application connected to Copilot, by copying the
-- template and filling additional necessary files.
rosApp' :: FilePath                           -- ^ Target directory where the
                                              -- application should be created.
        -> [String]                           -- ^ List of variable names
                                              -- (data sources).
        -> [(String, String, String, String)] -- ^ List of variables with their
                                              -- types, and the message IDs
                                              -- (topics) they can be obtained
                                              -- from.
        -> [String]                           -- ^ List of handlers associated
                                              -- to the monitors (or
                                              -- requirements monitored).
       -> [String]                            -- ^ Additional applications to turn on during testing
       -> [String]                            -- ^ Limited list of variables to use for testing
        -> IO (Either ErrorTriplet ())
rosApp' targetDir varNames varDB monitors testingAdditionalApps testingLimitedVars =
  E.handle (return . Left . cannotCopyTemplate) $ do
    -- Obtain template dir
    dataDir <- getDataDir
    let templateDir = dataDir </> "templates" </> "ros"

    -- Expand template
    copyDirectoryRecursive templateDir targetDir

    let f n o@(oVars, oIds, oInfos, oDatas) =
          case variableMap varDB n of
            Nothing -> o
            Just (vars, ids, infos, datas) ->
              ( vars : oVars
              , ids : oIds
              , infos : oInfos
              , datas : oDatas
              )

    -- This is a Data.List.unzip4
    let (vars, ids, infos, datas) =
          foldr f ([], [], [], []) varNames

    let rosFileName =
          targetDir </> "copilot" </> "src" </> "copilot_monitor.cpp"
        rosFileContents =
          unlines $
            rosMonitorContents varNames vars ids infos datas monitors

    writeFile rosFileName rosFileContents

    let rosFileName =
          targetDir </> "copilot" </> "src" </> "copilot_logger.cpp"
        rosFileContents =
          unlines $
            rosLoggerContents varNames vars ids infos datas monitors

    writeFile rosFileName rosFileContents

    let rosFileName =
          targetDir </> "test_requirements" </> "src" </> "test_requirements.cpp"

        limitedVars = if null testingLimitedVars then varNames else testingLimitedVars
        rosFileContents =
          unlines $ testClass varNames vars ids infos datas monitors testingAdditionalApps limitedVars

    writeFile rosFileName rosFileContents

    return $ Right ()

-- ** Argument processing

-- | Process FRET component spec, if available, and return its abstract
-- representation.
parseOptionalFRETCS :: Maybe FilePath
                    -> ExceptT ErrorTriplet IO (Maybe (Spec String))
parseOptionalFRETCS Nothing   = return Nothing
parseOptionalFRETCS (Just fp) = do
  -- Throws an exception if the file cannot be read.
  content <- liftIO $ B.safeReadFile fp
  let fretCS :: Either String (Spec String)
      fretCS = parseJSONSpec return fretFormat =<< eitherDecode =<< content

  case fretCS of
    Left e   -> throwError $ cannotOpenFRETFile fp e
    Right cs -> return $ Just cs

-- | Process a variable selection file, if available, and return the variable
-- names.
parseOptionalVariablesFile :: Maybe FilePath
                           -> ExceptT ErrorTriplet IO (Maybe [String])
parseOptionalVariablesFile Nothing = return Nothing
parseOptionalVariablesFile (Just fp) = do
  -- Fail if the file cannot be opened.
  varNamesE <- liftIO $ E.try $ lines <$> readFile fp
  case varNamesE of
    Left e         -> throwError $ cannotOpenVarFile fp e
    Right varNames -> return $ Just varNames

-- | Process a requirements / handlers list file, if available, and return the
-- handler names.
parseOptionalRequirementsListFile :: Maybe FilePath
                                  -> ExceptT ErrorTriplet IO (Maybe [String])
parseOptionalRequirementsListFile Nothing = return Nothing
parseOptionalRequirementsListFile (Just fp) = do
  -- Fail if the file cannot be opened.
  handlerNamesE <- liftIO $ E.try $ lines <$> readFile fp
  case handlerNamesE of
    Left e         -> throwError $ cannotOpenHandlersFile fp e
    Right monitors -> return $ Just monitors

-- | Process a variable database file, if available, and return the rows in it.
parseOptionalVarDBFile :: Maybe FilePath
                       -> ExceptT ErrorTriplet
                                  IO
                                  [(String, String, String, String)]
parseOptionalVarDBFile Nothing   = return []
parseOptionalVarDBFile (Just fp) = do
  -- We first try to open the files we need to fill in details in the ROS app
  -- template.
  --
  -- The variable DB is optional, so this check only fails if the filename
  -- provided does not exist or if the file cannot be opened or parsed (wrong
  -- format).
  varDBE <- liftIO $ E.try $ fmap read <$> lines <$> readFile fp
  case varDBE of
    Left  e     -> throwError $ cannotOpenDB fp e
    Right varDB -> return varDB

-- | Check that the arguments provided are sufficient to operate.
--
-- The ROS backend provides several modes of operation, which are selected
-- by providing different arguments to the `ros` command.
--
-- When a FRET component specification file is provided, the variables and
-- requirements defined in it are used unless variables or handlers files are
-- provided, in which case the latter take priority.
--
-- If a FRET file is not provided, then the user must provide BOTH a variable
-- list, and a list of handlers.
checkArguments :: Maybe (Spec String)
               -> Maybe [String]
               -> Maybe [String]
               -> Either ErrorTriplet ()
checkArguments Nothing Nothing   Nothing   = Left wrongArguments
checkArguments Nothing Nothing   _         = Left wrongArguments
checkArguments Nothing _         Nothing   = Left wrongArguments
checkArguments _       (Just []) _         = Left wrongArguments
checkArguments _       _         (Just []) = Left wrongArguments
checkArguments _       _         _         = Right ()

-- | Extract the variables from a FRET component specification, and sanitize
-- them to be used in ROS.
fretCSExtractExternalVariables :: Maybe (Spec String) -> [String]
fretCSExtractExternalVariables Nothing   = []
fretCSExtractExternalVariables (Just cs) = map sanitizeLCIdentifier
                                         $ map externalVariableName
                                         $ externalVariables cs

-- | Extract the requirements from a FRET component specification, and sanitize
-- them to match the names of the handlers used by Copilot.
fretCSExtractHandlers :: Maybe (Spec String) -> [String]
fretCSExtractHandlers Nothing   = []
fretCSExtractHandlers (Just cs) = map handlerNameF
                                $ map requirementName
                                $ requirements cs
  where
    handlerNameF = ("handler" ++) . sanitizeUCIdentifier

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

    -- Convert a DB row into Variable info needed to generate the ROS file
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

-- * ROS apps content

-- | Return the contents of the main ROS application.
rosMonitorContents :: [String]     -- Variables
                   -> [VarDecl]
                   -> [MsgInfoId]
                   -> [MsgInfo]
                   -> [MsgData]
                   -> [String]     -- Monitors
                   -> [String]
rosMonitorContents varNames variables msgIds msgNames msgDatas monitors =
    [ "#include <functional>"
    , "#include <memory>"
    , ""
    , "#include \"rclcpp/rclcpp.hpp\""
    , ""
    , typeIncludes
    , copilotIncludes
    , "using std::placeholders::_1;"
    , ""
    , variablesS
    , "class CopilotRV : public rclcpp::Node {"
    , "  public:"
    , "    CopilotRV() : Node(\"copilotrv\") {"
    , msgSubscriptionS
    , msgPublisherS
    , "    }"
    , ""
    , msgHandlerInClassS
    , "    // Needed so we can report messages to the log."
    , "    static CopilotRV& getInstance() {"
    , "      static CopilotRV instance;"
    , "      return instance;"
    , "    }"
    , ""
    , "  private:"
    , msgCallbacks
    , msgSubscriptionDeclrs
    , msgPublisherDeclrs
    , "};"
    , ""
    , msgHandlerGlobalS
    , "int main(int argc, char* argv[]) {"
    , "  rclcpp::init(argc, argv);"
    , "  rclcpp::spin(std::make_shared<CopilotRV>());"
    , "  rclcpp::shutdown();"
    , "  return 0;"
    , "}"
    ]

  where

    msgHandlerInClassS = unlines
                       $ concat
                       $ intersperse [""]
                       $ map msgHandlerInClass monitors
    msgHandlerInClass monitor =
        [ "    // Report (publish) monitor violations."
        , "    void " ++ handlerName ++ "() {"
        , "      auto output = " ++ ty ++ "();"
        , "      " ++ publisher ++ "->publish(output);"
        , "    }"
        ]
      where
        handlerName :: String
        handlerName = monitor

        ty = "std_msgs::msg::Empty"

        publisher = monitor ++ "_publisher_"

    typeIncludes = unlines
      [ "#include \"std_msgs/msg/bool.hpp\""
      , "#include \"std_msgs/msg/empty.hpp\""
      , "#include \"std_msgs/msg/u_int8.hpp\""
      , "#include \"std_msgs/msg/u_int16.hpp\""
      , "#include \"std_msgs/msg/u_int32.hpp\""
      , "#include \"std_msgs/msg/u_int64.hpp\""
      , "#include \"std_msgs/msg/int8.hpp\""
      , "#include \"std_msgs/msg/int16.hpp\""
      , "#include \"std_msgs/msg/int32.hpp\""
      , "#include \"std_msgs/msg/int64.hpp\""
      , "#include \"std_msgs/msg/float32.hpp\""
      , "#include \"std_msgs/msg/float64.hpp\""
      , "#include <cstdint>"
      ]

    copilotIncludes = unlines
      [ "#include \"monitor.h\""
      , "#include \"monitor.c\""
      ]

    variablesS = unlines $ map toVarDecl variables
    toVarDecl varDecl =
        varDeclType' varDecl ++ " " ++ varDeclName varDecl ++ ";"
      where
        varDeclType' varDecl = case varDeclType varDecl of
          "uint8_t"  -> "std::uint8_t"
          "uint16_t" -> "std::uint16_t"
          "uint32_t" -> "std::uint32_t"
          "uint64_t" -> "std::uint64_t"
          "int8_t"   -> "std::int8_t"
          "int16_t"  -> "std::int16_t"
          "int32_t"  -> "std::int32_t"
          "int64_t"  -> "std::int64_t"
          "float"    -> "float"
          "double"   -> "double"
          def        -> def

    msgSubscriptionS     = unlines
                         $ concat
                         $ intersperse [""]
                         $ map toMsgSubscription (zip variables msgIds)
    toMsgSubscription (nm, msgId) =
        [ "      " ++ subscription
                   ++ " = this->create_subscription<" ++ ty ++ ">("
        , "        \"" ++ topic ++ "\", " ++ show unknownVar ++ ","
        , "        std::bind(&CopilotRV::" ++ callback ++ ", this, _1));"
        ]
      where
        ty           = varDeclMsgType nm
        topic        = msgId
        subscription = varDeclName nm ++ "_subscription_"
        callback     = varDeclName nm ++ "_callback"

        unknownVar   :: Int
        unknownVar   = 10

    msgPublisherS = unlines
                  $ concat
                  $ intersperse [""]
                  $ map toMsgPublisher monitors

    toMsgPublisher nm =
        [ "      " ++ publisher
                   ++ " = this->create_publisher<" ++ ty ++ ">("
        , "        \"" ++ topic ++ "\", " ++ show unknownVar ++ ");"
        ]
      where
        ty        = "std_msgs::msg::Empty"
        publisher = nm ++ "_publisher_"
        topic     = "copilot/" ++ nm

        unknownVar   :: Int
        unknownVar   = 10

    varDeclMsgType varDecl = case varDeclType varDecl of
      "bool"     -> "std_msgs::msg::Bool"
      "uint8_t"  -> "std_msgs::msg::UInt8"
      "uint16_t" -> "std_msgs::msg::UInt16"
      "uint32_t" -> "std_msgs::msg::UInt32"
      "uint64_t" -> "std_msgs::msg::UInt64"
      "int8_t"   -> "std_msgs::msg::Int8"
      "int16_t"  -> "std_msgs::msg::Int16"
      "int32_t"  -> "std_msgs::msg::Int32"
      "int64_t"  -> "std_msgs::msg::Int64"
      "float"    -> "std_msgs::msg::Float32"
      "double"   -> "std_msgs::msg::Float64"
      def        -> def

    msgCallbacks = unlines
                 $ concat
                 $ intersperse [""]
                 $ map toCallback variables
    toCallback varDecl =
        [ "    void " ++ callback
                      ++ "(const " ++ ty ++ "::SharedPtr msg) const {"
        , "      " ++ variable ++ " = msg->data;"
        , "      step();"
        , "    }"
        ]
      where
        ty = varDeclMsgType varDecl
        variable = varDeclName varDecl
        callback = variable ++ "_callback"

    msgHandlerGlobalS = unlines
                      $ concat
                      $ intersperse [""]
                      $ map msgHandlerGlobal monitors
    msgHandlerGlobal monitor =
        [ "// Pass monitor violations to the actual class, which has ways to"
        , "// communicate with other applications."
        , "void " ++ handlerName ++ "() {"
        , "  CopilotRV::getInstance()." ++ handlerName ++ "();"
        , "}"
        ]
      where
        handlerName = monitor

    msgSubscriptionDeclrs :: String
    msgSubscriptionDeclrs = unlines
                          $ concat
                          $ intersperse [""]
                          $ map toSubscriptionDecl variables
    toSubscriptionDecl nm =
        [ "    rclcpp::Subscription<" ++ ty ++ ">::SharedPtr "
            ++ subscription ++ ";"
        ]
      where
        ty           = varDeclMsgType nm
        subscription = varDeclName nm ++ "_subscription_"

    msgPublisherDeclrs :: String
    msgPublisherDeclrs = unlines
                       $ concat
                       $ intersperse [""]
                       $ map toPublisherDecl monitors
    toPublisherDecl nm =
        [ "    rclcpp::Publisher<" ++ ty ++ ">::SharedPtr "
            ++ publisher ++ ";"
        ]
      where
        ty        = "std_msgs::msg::Empty"
        publisher = nm ++ "_publisher_"

-- | Return the contents of the logger ROS application.
rosLoggerContents :: [String]     -- Variables
                  -> [VarDecl]
                  -> [MsgInfoId]
                  -> [MsgInfo]
                  -> [MsgData]
                  -> [String]     -- Monitors
                  -> [String]
rosLoggerContents varNames variables msgIds msgNames msgDatas monitors =
    rosFileContents

  where

    rosFileContents =
      [ "#include <functional>"
      , "#include <memory>"
      , ""
      , "#include \"rclcpp/rclcpp.hpp\""
      , ""
      , typeIncludes
      , "using std::placeholders::_1;"
      , ""
      , "class CopilotLogger : public rclcpp::Node {"
      , "  public:"
      , "    CopilotLogger() : Node(\"copilotlogger\") {"
      , msgSubscriptionS
      , "    }"
      , ""
      , "  private:"
      , msgCallbacks
      , msgSubscriptionDeclrs
      , "};"
      , ""
      , "int main(int argc, char* argv[]) {"
      , "  rclcpp::init(argc, argv);"
      , "  rclcpp::spin(std::make_shared<CopilotLogger>());"
      , "  rclcpp::shutdown();"
      , "  return 0;"
      , "}"
      ]

    typeIncludes = unlines
      [ "#include \"std_msgs/msg/empty.hpp\""
      ]

    msgSubscriptionS     = unlines
                         $ concat
                         $ intersperse [""]
                         $ map toMsgSubscription monitors
    toMsgSubscription nm =
        [ "      " ++ subscription
                   ++ " = this->create_subscription<" ++ ty ++ ">("
        , "        \"" ++ topic ++ "\", " ++ show unknownVar ++ ","
        , "        std::bind(&CopilotLogger::" ++ callback ++ ", this, _1));"
        ]
      where
        ty           = "std_msgs::msg::Empty"
        topic        = "copilot/" ++ nm
        subscription = nm ++ "_subscription_"
        callback     = nm ++ "_callback"

        unknownVar   :: Int
        unknownVar   = 10

    msgCallbacks = unlines
                 $ concat
                 $ intersperse [""]
                 $ map toCallback monitors
    toCallback varDecl =
        [ "    void " ++ callback
                      ++ "(const " ++ ty ++ "::SharedPtr msg) const {"
        , "      RCLCPP_INFO(this->get_logger(), \"Copilot monitor violation: "
             ++ varDecl ++ "\");"
        , "    }"
        ]
      where
        ty = "std_msgs::msg::Empty"
        callback = varDecl ++ "_callback"

    msgSubscriptionDeclrs :: String
    msgSubscriptionDeclrs = unlines
                          $ concat
                          $ intersperse [""]
                          $ map toSubscriptionDecl monitors
    toSubscriptionDecl nm =
        [ "    rclcpp::Subscription<" ++ ty ++ ">::SharedPtr "
            ++ subscription ++ ";"
        ]
      where
        ty           = "std_msgs::msg::Empty"
        subscription = nm ++ "_subscription_"

-- * Exception handlers

-- | Exception handler to deal with the case in which the arguments
-- provided are incorrect.
wrongArguments :: ErrorTriplet
wrongArguments =
    ErrorTriplet ecWrongArguments msg LocationNothing
  where
    msg =
      "the arguments provided are insufficient: you must provide a FRET "
      ++ "component specification file, or both a variables and a handlers "
      ++ "file."

-- | Exception handler to deal with the case in which the FRET CS cannot be
-- opened.
cannotOpenFRETFile :: FilePath -> String -> ErrorTriplet
cannotOpenFRETFile file _e =
    ErrorTriplet ecCannotOpenFRETFile msg (LocationFile file)
  where
    msg =
      "cannot open FRET component specification file " ++ file

-- | Exception handler to deal with the case in which the variable DB cannot be
-- opened.
cannotOpenDB :: FilePath -> E.SomeException -> ErrorTriplet
cannotOpenDB file _e =
    ErrorTriplet ecCannotOpenDBUser msg (LocationFile file)
  where
    msg =
      "cannot open variable DB file " ++ file

-- | Exception handler to deal with the case in which the variable file
-- provided by the user cannot be opened.
cannotOpenVarFile :: FilePath -> E.SomeException -> ErrorTriplet
cannotOpenVarFile file _e =
    ErrorTriplet ecCannotOpenVarFile  msg (LocationFile file)
  where
    msg =
      "cannot open variable list file " ++ file

-- | Exception handler to deal with the case in which the handlers file
-- provided by the user cannot be opened.
cannotOpenHandlersFile :: FilePath -> E.SomeException -> ErrorTriplet
cannotOpenHandlersFile file _e =
    ErrorTriplet ecCannotOpenHandlersFile  msg (LocationFile file)
  where
    msg =
      "cannot open handler list file " ++ file

-- | Exception handler to deal with the case of files that cannot be
-- copied/generated due lack of space or permissions or some I/O error.
cannotCopyTemplate :: E.SomeException -> ErrorTriplet
cannotCopyTemplate e =
    ErrorTriplet ecCannotCopyTemplate msg LocationNothing
  where
    msg =
      "ROS app generation failed during copy/write operation. Check that"
      ++ " there's free space in the disk and that you have the necessary"
      ++ " permissions to write in the destination directory."
      ++ show e

-- | A triplet containing error information.
data ErrorTriplet = ErrorTriplet ErrorCode String Location

-- | Process a computation that can fail with an error code, and turn it into a
-- computation that returns a 'Result'.
processResult :: Monad m => ExceptT ErrorTriplet m a -> m (Result ErrorCode)
processResult m = do
  r <- runExceptT m
  case r of
    Left (ErrorTriplet errorCode msg location)
      -> return $ Error errorCode msg location
    _ -> return Success

-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error codes used are 1 for user error, and 2 for internal bug.
type ErrorCode = Int

-- | Error: wrong arguments provided.
ecWrongArguments :: ErrorCode
ecWrongArguments = 1

-- | Error: the FRET component specification provided by the user cannot be
-- opened.
ecCannotOpenFRETFile :: ErrorCode
ecCannotOpenFRETFile = 1

-- | Error: the variable DB provided by the user cannot be opened.
ecCannotOpenDBUser :: ErrorCode
ecCannotOpenDBUser = 1

-- | Error: the variable file provided by the user cannot be opened.
ecCannotOpenVarFile :: ErrorCode
ecCannotOpenVarFile = 1

-- | Error: the handlers file provided by the user cannot be opened.
ecCannotOpenHandlersFile :: ErrorCode
ecCannotOpenHandlersFile = 1

-- | Error: the files cannot be copied/generated due lack of space or
-- permissions or some I/O error.
ecCannotCopyTemplate :: ErrorCode
ecCannotCopyTemplate = 1

-- | JSONPath selectors for a FRET file
fretFormat :: JSONFormat
fretFormat = JSONFormat
  { specInternalVars    = Just "..Internal_variables[*]"
  , specInternalVarId   = ".name"
  , specInternalVarExpr = ".assignmentCopilot"
  , specInternalVarType = Just ".type"
  , specExternalVars    = Just "..Other_variables[*]"
  , specExternalVarId   = ".name"
  , specExternalVarType = Just ".type"
  , specRequirements    = "..Requirements[*]"
  , specRequirementId   = ".name"
  , specRequirementDesc = Just ".fretish"
  , specRequirementExpr = ".ptLTL"
  }

testClass :: [String]
          -> [VarDecl]
          -> [MsgInfoId]
          -> [MsgInfo]
          -> [MsgData]
          -> [String]     -- Monitors
          -> [String]     -- apps
          -> [String]     -- vars
          -> [String]
testClass varNames variables msgIds msgNames msgDatas monitors otherApps limitedVars =
    [ "#include <functional>"
    , "#include <memory>"
    , ""
    , "#include \"gtest/gtest.h\""
    , ""
    , "#include \"rclcpp/rclcpp.hpp\""
    , ""
    , "#include \"std_msgs/msg/bool.hpp\""
    , "#include \"std_msgs/msg/empty.hpp\""
    , "#include \"std_msgs/msg/u_int8.hpp\""
    , "#include \"std_msgs/msg/u_int16.hpp\""
    , "#include \"std_msgs/msg/u_int32.hpp\""
    , "#include \"std_msgs/msg/u_int64.hpp\""
    , "#include \"std_msgs/msg/int8.hpp\""
    , "#include \"std_msgs/msg/int16.hpp\""
    , "#include \"std_msgs/msg/int32.hpp\""
    , "#include \"std_msgs/msg/int64.hpp\""
    , "#include \"std_msgs/msg/float32.hpp\""
    , "#include \"std_msgs/msg/float64.hpp\""
    , "#include <cstdint>"
    , ""
    , "using std::placeholders::_1;"
    , ""
    , "class RequirementsTest : public rclcpp::Node {"
    , "  public:"
    , "    RequirementsTest() : Node(\"requirementstest\") {"
    , ""
    , "      declare_parameter(\"testing_seed\", 0); // defaults to 0"
    , "      declare_parameter(\"testing_deadline\", 2); // defaults to 2 secs"
    , ""
    ]
    ++ publisherConstructors
    ++ subscriptionConstructors
    ++
    [ ""
    , "      get_parameter(\"testing_seed\", initial_seed);"
    , "      get_parameter(\"testing_deadline\", deadline);"
    , ""
    , "      std::srand((unsigned int)this->initial_seed);"
    , ""
    , "      this->seed = this->initial_seed;"
    , "      this->max_tests = calculate_num_tests();"
    , ""
    , "      rclcpp::Duration update_period = rclcpp::Duration::from_seconds(1);"
    , "      timerInit = rclcpp::create_timer(this->get_node_base_interface(),"
    , "                                       this->get_node_timers_interface(),"
    , "                                       this->get_node_clock_interface()->get_clock(),"
    , "                                       update_period,"
    , "                                       std::bind(&RequirementsTest::tests_init, this)"
    , "                                       );"
    , "    }"
    , ""
    , "  private:"
    , ""
    ]
    ++ violationDecls ++
    [ "    bool violations = false;"
    , ""
    ]
    ++ publisherDeclrs
    ++ [ "" ]
    ++ handlerDecls
    ++ subscriptionDecls
    ++
    [ ""
    , "    int initial_seed; // To be configured using a parameter."
    , "    int seed;         // To be configured using a parameter."
    , "    int deadline;     // To be configured using a parameter."
    , ""
    , "    int max_tests;"
    , "    int num_test = 0;"
    , ""
    , "    // Calculate the number of tests to be executed"
    , "    int calculate_num_tests() {"
    , "       return abs(std::rand());"
    , "    }"
    , ""
    , "    rclcpp::TimerBase::SharedPtr timerResult;"
    , "    rclcpp::TimerBase::SharedPtr timerInit;"
    , ""
    , "    void tests_init () {"
    , "        timerInit->cancel();"
    , "        tests_step_send();"
    , "    }"
    , ""
    , "    void tests_step_send () {"
    ]
    ++ msgPublishers ++
    [ ""
    , "        rclcpp::Duration update_period = rclcpp::Duration::from_seconds(deadline);"
    , "        timerResult = rclcpp::create_timer(this->get_node_base_interface(),"
    , "                                           this->get_node_timers_interface(),"
    , "                                           this->get_node_clock_interface()->get_clock(),"
    , "                                           update_period,"
    , "                                           std::bind(&RequirementsTest::tests_step_result, this)"
    , "                                           );"
    , "    }"
    , ""
    , "    void tests_step_result () {"
    , "        timerResult->cancel();"
    ]
    ++ violationDetectors ++
    [ ""
    , "       this->num_test++;"
    , ""
    , "       // Stop if out of steps or there have been violations"
    , "       if ((this->num_test >= this->max_tests) || violations) {"
    , "         // Terminate using the gtest mechanism to indicate the result"
    , "         if (violations) {"
    , "           RCLCPP_INFO(this->get_logger(), \"Tests failed\");"
    , "           // FAIL();"
    , "         } else {"
    , "           RCLCPP_INFO(this->get_logger(), \"Tests succeeded\");"
    , "           // SUCCEED();"
    , "         }"
    , "         rclcpp::shutdown();"
    , "       } else {"
    , "         tests_step_send();"
    , "       }"
    , "    }"
    , ""
    , "    float randomFloat() {"
    , "       int numerator = rand();"
    , "       int denominator = rand();"
    , ""
    , "       // Ensure that we do not divide by zero."
    , "       if (denominator == 0) {"
    , "           denominator = 1;"
    , "       }"
    , ""
    , "       return (float)numerator / (float)denominator;"
    , "    }"
    , ""
    , "    int randomInt() {"
    , "       return rand();"
    , "    }"
    , ""
    , "    bool randomBool() {"
    , "       return rand() & 1;"
    , "    }"
    , ""
    , "    void delay(int time) {"
    , "       rclcpp::sleep_for(std::chrono::seconds(time));"
    , "    }"
    , ""
    , "    void publish_violation (char* requirement) {"
    , "        RCLCPP_INFO(this->get_logger(), \"Requirement violation. Req: %s; Seed: %d; Step: %d\\n\","
    , "            requirement, this->initial_seed, this->num_test);"
    , "    }"
    , "};"
    , ""
    , "int main(int argc, char* argv[]) {"
    , "  rclcpp::init(argc, argv);"
    , "  rclcpp::spin(std::make_shared<RequirementsTest>());"
    , "  rclcpp::shutdown();"
    , "  return 0;"
    , "}"
    ]

  where

    publisherConstructors =
        concatMap toMsgPublisher vars
      where
        vars = filter (\(x, _) -> (varDeclName x `elem` limitedVars)) $ zip variables msgIds

        toMsgPublisher (nm, msgId) =
            [ "      " ++ publisher
                       ++ " = this->create_publisher<" ++ ty ++ ">("
            , "        \"" ++ topic ++ "\", " ++ show unknownVar ++ ");"
            , ""
            ]
          where
            ty        = varDeclMsgType nm
            publisher = varDeclName nm ++ "_publisher_"
            topic     = msgId

            unknownVar   :: Int
            unknownVar   = 10

    subscriptionConstructors :: [String]
    subscriptionConstructors =
        msgSubscriptionS
      where
        msgSubscriptionS :: [String]
        msgSubscriptionS     = concat
                             $ intersperse [""]
                             $ map toMsgSubscription monitors
        toMsgSubscription nm =
            [ "      " ++ subscription
                       ++ " = this->create_subscription<" ++ ty ++ ">("
            , "        \"" ++ topic ++ "\", " ++ show unknownVar ++ ","
            , "        std::bind(&RequirementsTest::" ++ callback ++ ", this, _1));"
            ]
          where
            ty           = "std_msgs::msg::Empty"
            topic        = "copilot/" ++ nm
            subscription = nm ++ "_subscription_"
            callback     = nm ++ "_callback"

            unknownVar   :: Int
            unknownVar   = 10

    publisherDeclrs =
        msgPublisherDeclrs
      where

        msgPublisherDeclrs :: [String]
        msgPublisherDeclrs = concat
                           $ intersperse [""]
                           $ map toPublisherDecl
                           $ filter (\x -> (varDeclName x `elem` limitedVars))
                           $ variables
        toPublisherDecl nm =
            [ "    rclcpp::Publisher<" ++ ty ++ ">::SharedPtr "
                ++ publisher ++ ";"
            ]
          where
            ty        = varDeclMsgType nm
            publisher = varDeclName nm ++ "_publisher_"

    handlerDecls = concat
                 $ intersperse [""]
                 $ map toCallback monitors
      where
        toCallback varDecl =
            [ "    void " ++ callback
                          ++ "(const " ++ ty ++ "::SharedPtr msg) {"
            , "        this->violation_" ++ varDecl ++ " = true;"
            , "        this->violations = true;"
            , "    }"
            ]
          where
            ty = "std_msgs::msg::Empty"
            callback = varDecl ++ "_callback"

    subscriptionDecls =
        map (\x -> "    rclcpp::Subscription<std_msgs::msg::Empty>::SharedPtr " ++ x ++ "_subscription_;" ) monitors
      where

    msgPublishers =
        concatMap msgPublisher vars
      where
        msgPublisher (ctype, varname, randomType, msgType, publisherName) =
             [ "       " ++ ctype ++ " " ++ varname ++ " = " ++ randomType ++ "();"
             , "       auto " ++ varname ++ "_msg = " ++ msgType ++ "();"
             , "       " ++ varname ++ "_msg.data = " ++ varname ++ ";"
             , "       " ++ publisherName ++ "->publish(" ++ varname ++ "_msg);"
             , ""
             ]

        vars = map (\decl -> (varDeclType decl, varDeclName decl ++ "_data", randomFunction decl, varDeclMsgType decl, varDeclName decl ++ "_publisher_"))
             $ filter (\x -> (varDeclName x `elem` limitedVars))
             $ variables


    violationDetectors =
        concatMap
          (\x -> [ "        if (this->violation_" ++ x ++ ") {"
                 , "            this->publish_violation(\"" ++ x ++ "\");"
                 , "        }"
                 ])
          monitors

    violationDecls =
        map (\x -> "    bool violation_" ++ x ++ " = false;" ) monitors

    varDeclMsgType varDecl = case varDeclType varDecl of
      "bool"     -> "std_msgs::msg::Bool"
      "uint8_t"  -> "std_msgs::msg::UInt8"
      "uint16_t" -> "std_msgs::msg::UInt16"
      "uint32_t" -> "std_msgs::msg::UInt32"
      "uint64_t" -> "std_msgs::msg::UInt64"
      "int8_t"   -> "std_msgs::msg::Int8"
      "int16_t"  -> "std_msgs::msg::Int16"
      "int32_t"  -> "std_msgs::msg::Int32"
      "int64_t"  -> "std_msgs::msg::Int64"
      "float"    -> "std_msgs::msg::Float32"
      "double"   -> "std_msgs::msg::Float64"
      def        -> def

    randomFunction varDecl = case varDeclType varDecl of
      "bool"     -> "randomBool"
      "uint8_t"  -> "randomInt"
      "uint16_t" -> "randomInt"
      "uint32_t" -> "randomInt"
      "uint64_t" -> "randomInt"
      "int8_t"   -> "randomInt"
      "int16_t"  -> "randomInt"
      "int32_t"  -> "randomInt"
      "int64_t"  -> "randomInt"
      "float"    -> "randomFloat"
      "double"   -> "randomFloat"
      def        -> def
