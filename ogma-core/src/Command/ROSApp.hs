{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Aeson           (eitherDecode, object, (.=))
import           Data.List            (find, intersperse)
import           Data.Maybe           (fromMaybe)
import           Data.Text.Lazy       (pack)
import           System.FilePath      ((</>))

-- External imports: auxiliary
import Data.ByteString.Extra  as B (safeReadFile)
import Data.String.Extra      (sanitizeLCIdentifier, sanitizeUCIdentifier)
import System.Directory.Extra (copyTemplate)

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
       -> Maybe FilePath -- ^ Directory where the template is to be found.
       -> Maybe FilePath -- ^ FRET Component specification file.
       -> Maybe FilePath -- ^ File containing a list of variables to make
                         --   available to Copilot.
       -> Maybe FilePath -- ^ File containing a list of known variables
                         --   with their types and the message IDs they
                         --   can be obtained from.
       -> Maybe FilePath -- ^ File containing a list of handlers used in the
                         --   Copilot specification. The handlers are assumed
                         --   to receive no arguments.
       -> IO (Result ErrorCode)
rosApp targetDir mTemplateDir fretCSFile varNameFile varDBFile handlersFile =
  processResult $ do
    cs    <- parseOptionalFRETCS fretCSFile
    vs    <- parseOptionalVariablesFile varNameFile
    rs    <- parseOptionalRequirementsListFile handlersFile
    varDB <- parseOptionalVarDBFile varDBFile

    liftEither $ checkArguments cs vs rs

    let varNames = fromMaybe (fretCSExtractExternalVariables cs) vs
        monitors = fromMaybe (fretCSExtractHandlers cs) rs

    e <- liftIO $ rosApp' targetDir mTemplateDir varNames varDB monitors
    liftEither e

-- | Generate a new ROS application connected to Copilot, by copying the
-- template and filling additional necessary files.
rosApp' :: FilePath                           -- ^ Target directory where the
                                              -- application should be created.
        -> Maybe FilePath                     -- ^ Directory where the template
                                              -- is to be found.
        -> [String]                           -- ^ List of variable names
                                              -- (data sources).
        -> [(String, String, String, String)] -- ^ List of variables with their
                                              -- types, and the message IDs
                                              -- (topics) they can be obtained
                                              -- from.
        -> [String]                           -- ^ List of handlers associated
                                              -- to the monitors (or
                                              -- requirements monitored).
        -> IO (Either ErrorTriplet ())
rosApp' targetDir mTemplateDir varNames varDB monitors =
  E.handle (return . Left . cannotCopyTemplate) $ do
    -- Obtain template dir
    templateDir <- case mTemplateDir of
                     Just x  -> return x
                     Nothing -> do
                       dataDir <- getDataDir
                       return $ dataDir </> "templates" </> "ros"

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

    let (variablesS,         msgSubscriptionS, msgPublisherS,
         msgHandlerInClassS, msgCallbacks,     msgSubscriptionDeclrs,
         msgPublisherDeclrs, msgHandlerGlobalS) =
          rosMonitorComponents varNames vars ids infos datas monitors

        (logMsgSubscriptionS, logMsgCallbacks, logMsgSubscriptionDeclrs) =
            rosLoggerComponents varNames vars ids infos datas monitors

        subst = object $
                  [ "variablesS"               .= pack variablesS
                  , "msgSubscriptionS"         .= pack msgSubscriptionS
                  , "msgPublisherS"            .= pack msgPublisherS
                  , "msgHandlerInClassS"       .= pack msgHandlerInClassS
                  , "msgCallbacks"             .= pack msgCallbacks
                  , "msgSubscriptionDeclrs"    .= pack msgSubscriptionDeclrs
                  , "msgPublisherDeclrs"       .= pack msgPublisherDeclrs
                  , "msgHandlerGlobalS"        .= pack msgHandlerGlobalS
                  , "logMsgSubscriptionS"      .= pack logMsgSubscriptionS
                  , "logMsgCallbacks"          .= pack logMsgCallbacks
                  , "logMsgSubscriptionDeclrs" .= pack logMsgSubscriptionDeclrs
                  ]

    copyTemplate templateDir subst targetDir

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
rosMonitorComponents
  :: [String]     -- Variables
  -> [VarDecl]
  -> [MsgInfoId]
  -> [MsgInfo]
  -> [MsgData]
  -> [String]     -- Monitors
  -> (String, String, String, String, String, String, String, String)
rosMonitorComponents varNames variables msgIds msgNames msgDatas monitors =
    ( variablesS
    , msgSubscriptionS
    , msgPublisherS
    , msgHandlerInClassS
    , msgCallbacks
    , msgSubscriptionDeclrs
    , msgPublisherDeclrs
    , msgHandlerGlobalS
    )

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
rosLoggerComponents :: [String]     -- Variables
                    -> [VarDecl]
                    -> [MsgInfoId]
                    -> [MsgInfo]
                    -> [MsgData]
                    -> [String]     -- Monitors
                    -> (String, String, String)
rosLoggerComponents varNames variables msgIds msgNames msgDatas monitors =
    (msgSubscriptionS, msgCallbacks, msgSubscriptionDeclrs)

  where

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
