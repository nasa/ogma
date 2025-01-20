{-# LANGUAGE ExistentialQuantification #-}
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
    ( rosApp
    , ErrorCode
    , ROSAppOptions(..)
    )
  where

-- External imports
import qualified Control.Exception    as E
import           Control.Monad.Except (ExceptT(..), liftEither, liftIO,
                                       runExceptT, throwError)
import           Data.Aeson           (eitherDecode, object, (.=))
import           Data.List            (isInfixOf, isPrefixOf, find, intersperse)
import           Data.Maybe           (fromMaybe)
import           Data.Text.Lazy       (pack)
import           System.Directory     (doesFileExist)
import           System.FilePath      ((</>))
import           System.Process       (readProcess)

-- External imports: auxiliary
import Data.ByteString.Extra  as B (safeReadFile)
import Data.String.Extra      (sanitizeLCIdentifier, sanitizeUCIdentifier)
import System.Directory.Extra (copyTemplate)

-- External imports: ogma
import Data.OgmaSpec            (Spec, externalVariableName, externalVariables,
                                 requirementName, requirements)
import Language.JSONSpec.Parser (JSONFormat (..), parseJSONSpec)
import Language.XMLSpec.Parser  (parseXMLSpec)

-- Internal imports: auxiliary
import Command.Result                 (Result (..))
import Data.Location                  (Location (..))

-- Internal imports: language ASTs, transformers
import qualified Language.CoCoSpec.AbsCoCoSpec as CoCoSpec
import qualified Language.CoCoSpec.ParCoCoSpec as CoCoSpec ( myLexer,
                                                             pBoolSpec )

import qualified Language.SMV.AbsSMV       as SMV
import qualified Language.SMV.ParSMV       as SMV (myLexer, pBoolSpec)
import           Language.SMV.Substitution (substituteBoolExpr)

import qualified Language.Trans.CoCoSpec2Copilot as CoCoSpec (boolSpec2Copilot,
                                                              boolSpecNames)
import           Language.Trans.SMV2Copilot      as SMV (boolSpec2Copilot,
                                                         boolSpecNames)

-- Internal imports
import Paths_ogma_core ( getDataDir )

-- * ROS app generation

-- | Generate a new ROS application connected to Copilot.
rosApp :: Maybe FilePath -- ^ Input specification file.
       -> ROSAppOptions  -- ^ Options to the ROS backend.
       -> IO (Result ErrorCode)
rosApp fp options =
    processResult $ do
      spec  <- parseOptionalInputFile
                 fp
                 options
                 (exprPair (rosAppPropFormat options))
      vs    <- parseOptionalVariablesFile varNameFile
      rs    <- parseOptionalRequirementsListFile handlersFile
      varDB <- parseOptionalVarDBFile varDBFile

      liftEither $ checkArguments spec vs rs

      let varNames = fromMaybe (specExtractExternalVariables spec) vs
          monitors = fromMaybe (specExtractHandlers spec) rs

      e <- liftIO $
             rosApp' targetDir mTemplateDir varNames varDB monitors
      liftEither e
  where
    targetDir    = rosAppTargetDir options
    mTemplateDir = rosAppTemplateDir options
    varNameFile  = rosAppVariables options
    varDBFile    = rosAppVariableDB options
    handlersFile = rosAppHandlers options

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

-- | Options used to customize the conversion of specifications to ROS
-- applications.
data ROSAppOptions = ROSAppOptions
  { rosAppTargetDir   :: FilePath       -- ^ Target directory where the
                                        -- application should be created.
  , rosAppTemplateDir :: Maybe FilePath -- ^ Directory where the template is
                                        -- to be found.
  , rosAppVariables   :: Maybe FilePath -- ^ File containing a list of
                                        -- variables to make available to
                                        -- Copilot.
  , rosAppVariableDB  :: Maybe FilePath -- ^ File containing a list of known
                                        -- variables with their types and the
                                        -- message IDs they can be obtained
                                        -- from.
  , rosAppHandlers    :: Maybe FilePath -- ^ File containing a list of
                                        -- handlers used in the Copilot
                                        -- specification. The handlers are
                                        -- assumed to receive no arguments.
  , rosAppFormat      :: String         -- ^ Format of the input file.
  , rosAppPropFormat  :: String         -- ^ Format used for input properties.
  , rosAppPropVia     :: Maybe String   -- ^ Use external command to
                                        -- pre-process system properties.
  }

-- | Process input specification, if available, and return its abstract
-- representation.
parseOptionalInputFile :: Maybe FilePath
                       -> ROSAppOptions
                       -> ExprPair
                       -> ExceptT ErrorTriplet IO (Maybe (Spec String))
parseOptionalInputFile Nothing   _    _ =
  return Nothing
parseOptionalInputFile (Just fp) opts (ExprPair parse replace print ids def) =
  ExceptT $ do
    let wrapper = wrapVia (rosAppPropVia opts) parse
    -- Obtain format file.
    --
    -- A format name that exists as a file in the disk always takes preference
    -- over a file format included with Ogma. A file format with a forward
    -- slash in the name is always assumed to be a user-provided filename.
    -- Regardless of whether the file is user-provided or known to Ogma, we
    -- check (again) whether the file exists, and print an error message if
    -- not.
    let formatName = rosAppFormat opts
    exists  <- doesFileExist formatName
    dataDir <- getDataDir
    let formatFile
          | isInfixOf "/" formatName || exists
          = formatName
          | otherwise
          = dataDir </> "data" </> "formats" </>
               (formatName ++ "_" ++ rosAppPropFormat opts)
    formatMissing <- not <$> doesFileExist formatFile

    if formatMissing
      then return $ Left $ rosAppIncorrectFormatSpec formatFile
      else do
        res <- do
          format <- readFile formatFile

          -- All of the following operations use Either to return error
          -- messages.  The use of the monadic bind to pass arguments from one
          -- function to the next will cause the program to stop at the
          -- earliest error.
          if | isPrefixOf "XMLFormat" format
             -> do let xmlFormat = read format
                   content <- readFile fp
                   parseXMLSpec
                     (fmap (fmap print) . wrapper) (print def) xmlFormat content
             | otherwise
             -> do let jsonFormat = read format
                   content <- B.safeReadFile fp
                   case content of
                     Left e  -> return $ Left e
                     Right b -> do case eitherDecode b of
                                     Left e  -> return $ Left e
                                     Right v ->
                                       parseJSONSpec
                                         (fmap (fmap print) . wrapper)
                                         jsonFormat
                                         v
        case res of
          Left e  -> return $ Left $ cannotOpenInputFile fp e
          Right x -> return $ Right $ Just x

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
-- When an input specification file is provided, the variables and requirements
-- defined in it are used unless variables or handlers files are provided, in
-- which case the latter take priority.
--
-- If an input file is not provided, then the user must provide BOTH a variable
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

-- | Extract the variables from a specification, and sanitize them to be used
-- in ROS.
specExtractExternalVariables :: Maybe (Spec String) -> [String]
specExtractExternalVariables Nothing   = []
specExtractExternalVariables (Just cs) = map sanitizeLCIdentifier
                                       $ map externalVariableName
                                       $ externalVariables cs

-- | Extract the requirements from a specification, and sanitize them to match
-- the names of the handlers used by Copilot.
specExtractHandlers :: Maybe (Spec String) -> [String]
specExtractHandlers Nothing   = []
specExtractHandlers (Just cs) = map handlerNameF
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

-- * Handler for boolean expressions

-- | Handler for boolean expressions that knows how to parse them, replace
-- variables in them, and convert them to Copilot.
--
-- It also contains a default value to be used whenever an expression cannot be
-- found in the input file.
data ExprPair = forall a . ExprPair
  { exprParse   :: String -> Either String a
  , exprReplace :: [(String, String)] -> a -> a
  , exprPrint   :: a -> String
  , exprIdents  :: a -> [String]
  , exprUnknown :: a
  }

-- | Return a handler depending on whether it should be for CoCoSpec boolean
-- expressions or for SMV boolean expressions. We default to SMV if not format
-- is given.
exprPair :: String -> ExprPair
exprPair "cocospec" =
  ExprPair
    (CoCoSpec.pBoolSpec . CoCoSpec.myLexer)
    (\_ -> id)
    (CoCoSpec.boolSpec2Copilot)
    (CoCoSpec.boolSpecNames)
    (CoCoSpec.BoolSpecSignal (CoCoSpec.Ident "undefined"))
exprPair "literal" =
  ExprPair
    Right
    (\_ -> id)
    id
    (const [])
    "undefined"
exprPair _ =
  ExprPair
    (SMV.pBoolSpec . SMV.myLexer)
    (substituteBoolExpr)
    (SMV.boolSpec2Copilot)
    (SMV.boolSpecNames)
    (SMV.BoolSpecSignal (SMV.Ident "undefined"))

-- | Parse a property using an auxiliary program to first translate it, if
-- available.
--
-- If a program is given, it is first called on the property, and then the
-- result is parsed with the parser passed as an argument. If a program is not
-- given, then the parser is applied to the given string.
wrapVia :: Maybe String                -- ^ Auxiliary program to translate the
                                       -- property.
        -> (String -> Either String a) -- ^ Parser used on the result.
        -> String                      -- ^ Property to parse.
        -> IO (Either String a)
wrapVia Nothing  parse s = return (parse s)
wrapVia (Just f) parse s =
  E.handle (\(e :: E.IOException) -> return $ Left $ show e) $ do
    out <- readProcess f [] s
    return $ parse out

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
      "the arguments provided are insufficient: you must provide an input "
      ++ "specification, or both a variables and a handlers file."

-- | Exception handler to deal with the case in which the input file cannot be
-- opened.
cannotOpenInputFile :: FilePath -> String -> ErrorTriplet
cannotOpenInputFile file _e =
    ErrorTriplet ecCannotOpenInputFile msg (LocationFile file)
  where
    msg =
      "cannot open input specification file " ++ file

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

-- | Error messages associated to the format file not being found.
rosAppIncorrectFormatSpec :: String -> ErrorTriplet
rosAppIncorrectFormatSpec formatFile =
    ErrorTriplet ecIncorrectFormatFile msg LocationNothing
  where
    msg =
      "The format specification " ++ formatFile ++ " does not exist or is not "
      ++ "readable"

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

-- | Error: the input specification provided by the user cannot be opened.
ecCannotOpenInputFile :: ErrorCode
ecCannotOpenInputFile = 1

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

-- | Error: the format file cannot be opened.
ecIncorrectFormatFile :: ErrorCode
ecIncorrectFormatFile = 1
