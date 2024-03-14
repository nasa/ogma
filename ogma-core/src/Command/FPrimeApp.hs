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
-- | Create <https://github.com/nasa/fprime FPrime> components that subscribe
-- to obtain data and call Copilot when new values arrive.

{- HLINT ignore "Functor law" -}
module Command.FPrimeApp
    ( fprimeApp
    , ErrorCode
    )
  where

-- External imports
import qualified Control.Exception    as E
import           Control.Monad.Except ( ExceptT, liftEither, liftIO, runExceptT,
                                        throwError )
import           Data.Aeson           ( eitherDecode )
import           Data.Char            ( toUpper )
import           Data.List            ( find, intercalate, nub, sort )
import           Data.Maybe           ( fromMaybe )
import           System.FilePath      ( (</>) )

-- External imports: auxiliary
import Data.ByteString.Extra  as B ( safeReadFile )
import Data.String.Extra      ( sanitizeLCIdentifier, sanitizeUCIdentifier )
import System.Directory.Extra ( copyDirectoryRecursive )

-- External imports: ogma
import Data.OgmaSpec            (Spec, externalVariableName, externalVariables,
                                 requirementName, requirements)
import Language.JSONSpec.Parser (JSONFormat (..), parseJSONSpec)

-- Internal imports: auxiliary
import Command.Result                 ( Result (..) )
import Data.Location                  ( Location (..) )

-- Internal imports
import Paths_ogma_core ( getDataDir )

-- * FPrime component generation

-- | Generate a new FPrime component connected to Copilot.
fprimeApp :: FilePath       -- ^ Target directory where the component
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
          -> IO (Result ErrorCode)
fprimeApp targetDir fretCSFile varNameFile varDBFile handlersFile =
  processResult $ do
    cs    <- parseOptionalFRETCS fretCSFile
    vs    <- parseOptionalVariablesFile varNameFile
    rs    <- parseOptionalRequirementsListFile handlersFile
    varDB <- parseOptionalVarDBFile varDBFile

    liftEither $ checkArguments cs vs rs

    let varNames = fromMaybe (fretCSExtractExternalVariables cs) vs
        monitors = fromMaybe (fretCSExtractHandlers cs) rs

    e <- liftIO $ fprimeApp' targetDir varNames varDB monitors
    liftEither e

-- | Generate a new FPrime component connected to Copilot, by copying the
-- template and filling additional necessary files.
fprimeApp' :: FilePath           -- ^ Target directory where the component
                                 -- should be created.
           -> [String]           -- ^ List of variable names (data sources).
           -> [(String, String)] -- ^ List of variables with their types, and
                                 -- the message IDs (topics) they can be
                                 -- obtained from.
           -> [String]           -- ^ List of handlers associated to the
                                 -- monitors (or requirements monitored).
           -> IO (Either ErrorTriplet ())
fprimeApp' targetDir varNames varDB monitors =
  E.handle (return . Left . cannotCopyTemplate) $ do
    -- Obtain template dir
    dataDir <- getDataDir
    let templateDir = dataDir </> "templates" </> "fprime"

    -- Expand template
    copyDirectoryRecursive templateDir targetDir

    let f n o@(oVars) =
          case variableMap varDB n of
            Nothing   -> o
            Just vars -> (vars : oVars)

    -- This is a Data.List.unzip4
    let vars = foldr f [] varNames

    let fprimeFileName =
          targetDir </> "Copilot.fpp"
        fprimeFileContents =
          unlines $
            componentInterface vars monitors

    writeFile fprimeFileName fprimeFileContents

    let fprimeFileName =
          targetDir </> "Copilot.hpp"
        fprimeFileContents =
          unlines $
            componentHeader vars monitors

    writeFile fprimeFileName fprimeFileContents

    let fprimeFileName =
          targetDir </> "Copilot.cpp"
        fprimeFileContents =
          unlines $
            componentImpl vars monitors

    writeFile fprimeFileName fprimeFileContents

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
parseOptionalVariablesFile Nothing   = return Nothing
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
parseOptionalRequirementsListFile Nothing   = return Nothing
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
                                  [(String, String)]
parseOptionalVarDBFile Nothing   = return []
parseOptionalVarDBFile (Just fp) = do
  -- We first try to open the files we need to fill in details in the FPrime
  -- component template.
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
-- The FPrime backend provides several modes of operation, which are selected
-- by providing different arguments to the `ros` command.
--
-- When a FRET component specification file is provided, the variables and
-- requirements defined in it are used unless variables or handlers files are
-- provided, in which case the latter take priority.
--
-- If a FRET file is not provided, then the user must provide BOTH a variable
-- list, and a list of handlers.
checkArguments :: Maybe (Spec a)
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
-- them to be used in FPrime.
fretCSExtractExternalVariables :: Maybe (Spec a) -> [String]
fretCSExtractExternalVariables Nothing   = []
fretCSExtractExternalVariables (Just cs) = map sanitizeLCIdentifier
                                         $ map externalVariableName
                                         $ externalVariables cs

-- | Extract the requirements from a FRET component specification, and sanitize
-- them to match the names of the handlers used by Copilot.
fretCSExtractHandlers :: Maybe (Spec a) -> [String]
fretCSExtractHandlers Nothing   = []
fretCSExtractHandlers (Just cs) = map handlerNameF
                                $ map requirementName
                                $ requirements cs
  where
    handlerNameF = ("handler" ++) . sanitizeUCIdentifier

-- | Return the variable information needed to generate declarations
-- and subscriptions for a given variable name and variable database.
variableMap :: [(String, String)]
            -> String
            -> Maybe VarDecl
variableMap varDB varName =
    csvToVarMap <$> find (sameName varName) varDB

  where

    -- True if the given variable and db entry have the same name
    sameName :: String
             -> (String, String)
             -> Bool
    sameName n (vn, _) = n == vn

    -- Convert a DB row into Variable info needed to generate the FPrime file
    csvToVarMap :: (String, String)
                -> (VarDecl)
    csvToVarMap (nm, ty) = (VarDecl nm ty)

-- | The declaration of a variable in C, with a given type and name.
data VarDecl = VarDecl
  { varDeclName :: String
  , varDeclType :: String
  }

-- * FPrime component content

-- | Return the contents of the FPrime component interface (.fpp) specification.
componentInterface :: [VarDecl]
                   -> [String]     -- Monitors
                   -> [String]
componentInterface variables monitors =
    [ "module Ref {"
    , ""
    ]
    ++ typePorts ++
    [ ""
    , "  @ Monitoring component"
    , "  queued component Copilot {"
    , ""
    , "    # ----------------------------------------------------------------------"
    , "    # General ports"
    , "    # ----------------------------------------------------------------------"
    , ""
    ]
    ++ inputPorts ++
    [ ""
    , "    # ----------------------------------------------------------------------"
    , "    # Special ports"
    , "    # ----------------------------------------------------------------------"
    , ""
    , "    @ Command receive"
    , "    command recv port cmdIn"
    , ""
    , "    @ Command registration"
    , "    command reg port cmdRegOut"
    , ""
    , "    @ Command response"
    , "    command resp port cmdResponseOut"
    , ""
    , "    @ Event"
    , "    event port eventOut"
    , ""
    , "    @ Parameter get"
    , "    param get port prmGetOut"
    , ""
    , "    @ Parameter set"
    , "    param set port prmSetOut"
    , ""
    , "    @ Telemetry"
    , "    telemetry port tlmOut"
    , ""
    , "    @ Text event"
    , "    text event port textEventOut"
    , ""
    , "    @ Time get"
    , "    time get port timeGetOut"
    , ""
    , "    # ----------------------------------------------------------------------"
    , "    # Parameters"
    , "    # ----------------------------------------------------------------------"
    , ""
    , "    # This section intentionally left blank"
    , ""
    , "    # ----------------------------------------------------------------------"
    , "    # Events"
    , "    # ----------------------------------------------------------------------"
    , ""
    ]
    ++ violationEvents ++
    [ ""
    , "    # ----------------------------------------------------------------------"
    , "    # Commands"
    , "    # ----------------------------------------------------------------------"
    , ""
    , "    sync command CHECK_MONITORS()"
    , ""
    , "    # ----------------------------------------------------------------------"
    , "    # Telemetry"
    , "    # ----------------------------------------------------------------------"
    , ""
    , "    # This section intentionally left blank"
    , ""
    , "  }"
    , ""
    , "}"
    ]
  where

    typePorts = nub $ sort $ map toTypePort variables
    toTypePort varDecl = "    port "
                       ++ fprimeVarDeclType varDecl
                       ++ "Value(value: "
                       ++ fprimeVarDeclType varDecl
                       ++ ")"

    inputPorts = map toInputPortDecl variables
    toInputPortDecl varDecl = "    async input port "
                            ++ varDeclName varDecl
                            ++ "In : " ++ fprimeVarDeclType varDecl
                            ++ "Value"

    fprimeVarDeclType varDecl = case varDeclType varDecl of
      "uint8_t"  -> "U8"
      "uint16_t" -> "U16"
      "uint32_t" -> "U32"
      "uint64_t" -> "U64"
      "int8_t"   -> "I8"
      "int16_t"  -> "I16"
      "int32_t"  -> "I32"
      "int64_t"  -> "I64"
      "float"    -> "F32"
      "double"   -> "F64"
      def        -> def

    violationEvents = intercalate [""]
                    $ map violationEvent monitors
    violationEvent monitor =
        [ "    @ " ++ monitor ++ " violation"
        , "    event " ++ ucMonitor ++ "_VIOLATION("
        , "          " ++ replicate (length ucMonitor) ' ' ++ "          ) \\"
        , "      severity activity high \\"
        , "      id 0 \\"
        , "      format \"" ++ monitor ++ " violation\""
        ]
      where
        ucMonitor = map toUpper monitor

-- | Return the contents of the FPrime component header file.
componentHeader :: [VarDecl]
                -> [String]     -- Monitors
                -> [String]
componentHeader variables _monitors =
    [ "// ======================================================================"
    , "// \\title  Copilot.hpp"
    , "// \\author root"
    , "// \\brief  hpp file for Copilot component implementation class"
    , "// ======================================================================"
    , ""
    , "#ifndef Copilot_HPP"
    , "#define Copilot_HPP"
    , ""
    , "#include \"Ref/Copilot/CopilotComponentAc.hpp\""
    , ""
    , "namespace Ref {"
    , ""
    , "  class Copilot :"
    , "    public CopilotComponentBase"
    , "  {"
    , ""
    , "    public:"
    , ""
    , "      // ----------------------------------------------------------------------"
    , "      // Construction, initialization, and destruction"
    , "      // ----------------------------------------------------------------------"
    , ""
    , "      //! Construct object Copilot"
    , "      //!"
    , "      Copilot("
    , "          const char *const compName /*!< The component name*/"
    , "      );"
    , ""
    , "      //! Initialize object Copilot"
    , "      //!"
    , "      void init("
    , "          const NATIVE_INT_TYPE queueDepth, /*!< The queue depth*/"
    , "          const NATIVE_INT_TYPE instance = 0 /*!< The instance number*/"
    , "      );"
    , ""
    , "      //! Destroy object Copilot"
    , "      //!"
    , "      ~Copilot();"
    , ""
    , "    PRIVATE:"
    , ""
    , "      // ----------------------------------------------------------------------"
    , "      // Handler implementations for user-defined typed input ports"
    , "      // ----------------------------------------------------------------------"
    , ""
    ]
    ++ handlers ++
    [ ""
    , "    PRIVATE:"
    , ""
    , "      // ----------------------------------------------------------------------"
    , "      // Command handler implementations"
    , "      // ----------------------------------------------------------------------"
    , ""
    , "      //! Implementation for CHECK_MONITORS command handler"
    , "      //! "
    , "      void CHECK_MONITORS_cmdHandler("
    , "          const FwOpcodeType opCode, /*!< The opcode*/"
    , "          const U32 cmdSeq /*!< The command sequence number*/"
    , "      );"
    , ""
    , "    };"
    , ""
    , "} // end namespace Ref"
    , ""
    , "#endif"
    ]
  where
    handlers = intercalate [""]
             $ map toInputHandler variables
    toInputHandler nm =
        [ "      //! Handler implementation for " ++ varDeclName nm ++ "In"
        , "      //!"
        , "      void " ++ varDeclName nm ++ "In_handler("
        , "            const NATIVE_INT_TYPE portNum, /*!< The port number*/"
        , "            " ++ portTy ++ " value"
        , "        );"
        ]
      where
        portTy = varDeclType nm


-- | Return the contents of the main FPrime component.
componentImpl :: [VarDecl]
              -> [String]     -- Monitors
              -> [String]
componentImpl variables monitors =
    [ "// ======================================================================"
    , "// \\title  Copilot.cpp"
    , "// \\author Ogma"
    , "// \\brief  cpp file for Copilot component implementation class"
    , "// ======================================================================"
    , ""
    , ""
    , "#include <Ref/Copilot/Copilot.hpp>"
    , "#include \"Fw/Types/BasicTypes.hpp\""
    , ""
    , "#ifdef __cplusplus"
    , "extern \"C\" {"
    , "#endif"
    , ""
    , "#include \"copilot.h\""
    , "#include \"copilot_types.h\""
    , ""
    , "#ifdef __cplusplus"
    , "}"
    , "#endif"
    , ""
    ]
    ++ inputs
    ++ monitorResults ++
    [ ""
    , "namespace Ref {"
    , ""
    , "  // ----------------------------------------------------------------------"
    , "  // Construction, initialization, and destruction"
    , "  // ----------------------------------------------------------------------"
    , ""
    , "  Copilot ::"
    , "    Copilot("
    , "        const char *const compName"
    , "    ) : CopilotComponentBase(compName)"
    , "  {"
    , ""
    , "  }"
    , ""
    , "  void Copilot ::"
    , "    init("
    , "        const NATIVE_INT_TYPE queueDepth,"
    , "        const NATIVE_INT_TYPE instance"
    , "    )"
    , "  {"
    , "    CopilotComponentBase::init(queueDepth, instance);"
    , "  }"
    , ""
    , "  Copilot ::"
    , "    ~Copilot()"
    , "  {"
    , ""
    , "  }"
    , ""
    , "  // ----------------------------------------------------------------------"
    , "  // Handler implementations for user-defined typed input ports"
    , "  // ----------------------------------------------------------------------"
    , ""
    ]
    ++ inputHandlers ++
    [ ""
    , "  // ----------------------------------------------------------------------"
    , "  // Command handler implementations"
    , "  // ----------------------------------------------------------------------"
    , ""
    , "  void Copilot ::"
    , "    CHECK_MONITORS_cmdHandler("
    , "        const FwOpcodeType opCode,"
    , "        const U32 cmdSeq"
    , "    )"
    , "  {"
    ]
    ++ triggerResultReset ++
    [ "    step();"
    , "    this->cmdResponse_out(opCode,cmdSeq,Fw::CmdResponse::OK);"
    ]
    ++ triggerChecks ++
    [ "  }"
    , ""
    , "} // end namespace Ref"
    , ""
    ]
    ++ triggers

  where

    inputs = variablesS

    monitorResults = intercalate [""]
                   $ map monitorResult monitors
    monitorResult monitor =
        [ "bool " ++ monitor ++ "_result;"
        ]

    inputHandlers = intercalate [""]
                  $ map toInputHandler variables
    toInputHandler nm =
        [ "  void Copilot :: "
        , "    " ++ varDeclName nm ++ "In_handler("
        , "        const NATIVE_INT_TYPE portNum,"
        , "        " ++ portTy ++ " value"
        , "    )"
        , "  {"
        , "    " ++ varDeclName nm ++ " = (" ++ ty ++ ") value;"
        , "  }"
        ]
      where
        portTy = varDeclType nm
        ty     = varDeclType nm

    triggerResultReset = intercalate [""]
                       $ map monitorResultReset monitors
    monitorResultReset monitor =
        [ "    " ++ monitor ++ "_result = false;"
        ]

    triggerChecks = intercalate [""]
                  $ map triggerCheck monitors
    triggerCheck monitor =
        [ "    if (" ++ monitor ++ "_result) {"
        , "       this->log_ACTIVITY_HI_" ++ ucMonitor ++ "_VIOLATION();"
        , "    }"
        ]
      where
        ucMonitor = map toUpper monitor

    triggers :: [String]
    triggers = intercalate [""]
             $ map triggerImpl monitors
    triggerImpl monitor =
        [ "void " ++ monitor ++ "() {"
        , "  " ++ monitor ++ "_result = true;"
        , "}"
        ]

    variablesS :: [String]
    variablesS = map toVarDecl variables
    toVarDecl varDecl =
      varDeclType varDecl ++ " " ++ varDeclName varDecl ++ ";"

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
      "FPrime component generation failed during copy/write operation. Check"
      ++ " that there's free space in the disk and that you have the necessary"
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
