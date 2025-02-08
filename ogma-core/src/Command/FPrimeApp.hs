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
-- | Create <https://github.com/nasa/fprime FPrime> components that subscribe
-- to obtain data and call Copilot when new values arrive.

{- HLINT ignore "Functor law" -}
module Command.FPrimeApp
    ( command
    , CommandOptions(..)
    , ErrorCode
    )
  where

-- External imports
import qualified Control.Exception      as E
import           Control.Monad.Except   ( ExceptT(..), liftEither, runExceptT,
                                          throwError )
import           Control.Monad.IO.Class ( liftIO )
import           Data.Aeson             ( ToJSON, eitherDecode, object, toJSON )
import           Data.Char              ( toUpper )
import           Data.List              ( isInfixOf, isPrefixOf, find )
import           Data.Maybe             ( fromMaybe, mapMaybe )
import           GHC.Generics           ( Generic )
import           System.Directory       ( doesFileExist )
import           System.FilePath        ( (</>) )
import           System.Process         ( readProcess )

-- External imports: auxiliary
import Data.ByteString.Extra  as B ( safeReadFile )
import Data.String.Extra      ( sanitizeLCIdentifier, sanitizeUCIdentifier )
import System.Directory.Extra ( copyTemplate )

-- External imports: ogma
import Data.OgmaSpec            (Spec, externalVariableName, externalVariables,
                                 requirementName, requirements)
import Language.JSONSpec.Parser (JSONFormat (..), parseJSONSpec)
import Language.XMLSpec.Parser  (parseXMLSpec)

-- External imports: language ASTs, transformers
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

-- Internal imports: auxiliary
import Command.Result  (Result (..))
import Data.Location   (Location (..))
import Paths_ogma_core (getDataDir)

-- | Generate a new FPrime component connected to Copilot.
command :: CommandOptions -- ^ Options to the ROS backend.
        -> IO (Result ErrorCode)
command options = processResult $ do
    -- Obtain template dir
    templateDir <- locateTemplateDir mTemplateDir "fprime"

    appData <- command' options functions

    let subst = toJSON appData

    -- Expand template
    ExceptT $ fmap (makeLeftE cannotCopyTemplate) $ E.try $
      copyTemplate templateDir subst targetDir

  where

    targetDir    = commandTargetDir options
    mTemplateDir = commandTemplateDir options
    functions    = exprPair (commandPropFormat options)

command' :: CommandOptions
         -> ExprPair
         -> ExceptT ErrorTriplet IO AppData
command' options (ExprPair exprT) = do
    -- Open files needed to fill in details in the template.
    vs    <- parseVariablesFile varNameFile
    rs    <- parseRequirementsListFile handlersFile
    varDB <- parseVarDBFile varDBFile

    spec  <- maybe (return Nothing)
                   (\f -> Just <$> parseInputFile f options exprT)
                   fp

    liftEither $ checkArguments spec vs rs

    let varNames = fromMaybe (specExtractExternalVariables spec) vs
        monitors = fromMaybe (specExtractHandlers spec) rs

    let appData   = AppData variables monitors'
        variables = mapMaybe (variableMap varDB) varNames
        monitors' = map (\x -> Monitor x (map toUpper x)) monitors

    return appData

  where

    fp           = commandInputFile options
    varNameFile  = commandVariables options
    varDBFile    = commandVariableDB options
    handlersFile = commandHandlers options

-- ** Argument processing

-- | Options used to customize the conversion of specifications to F'
-- applications.
data CommandOptions = CommandOptions
  { commandInputFile   :: Maybe FilePath -- ^ Input specification file.
  , commandTargetDir   :: FilePath       -- ^ Target directory where the
                                         -- component should be created.
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
  }

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
    csvToVarMap (nm, ty) = (VarDecl nm ty (fprimeVarDeclType ty))

    fprimeVarDeclType ty = case ty of
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

-- | The declaration of a variable in C, with a given type and name.
data VarDecl = VarDecl
    { varDeclName       :: String
    , varDeclType       :: String
    , varDeclFPrimeType :: String
    }
  deriving Generic

instance ToJSON VarDecl

data Monitor = Monitor
    { monitorName :: String
    , monitorUC   :: String
    }
  deriving Generic

instance ToJSON Monitor

-- | Data that may be relevant to generate a ROS application.
data AppData = AppData
  { variables :: [VarDecl]
  , monitors  :: [Monitor]
  }
  deriving (Generic)

instance ToJSON AppData

-- | Process input specification, if available, and return its abstract
-- representation.
parseInputFile :: FilePath
               -> CommandOptions
               -> ExprPairT a
               -> ExceptT ErrorTriplet IO (Spec a)
parseInputFile fp opts (ExprPairT parse replace print ids def) =
  ExceptT $ do
    let wrapper = wrapVia (commandPropVia opts) parse
    -- Obtain format file.
    --
    -- A format name that exists as a file in the disk always takes preference
    -- over a file format included with Ogma. A file format with a forward
    -- slash in the name is always assumed to be a user-provided filename.
    -- Regardless of whether the file is user-provided or known to Ogma, we
    -- check (again) whether the file exists, and print an error message if
    -- not.
    let formatName = commandFormat opts
    exists  <- doesFileExist formatName
    dataDir <- getDataDir
    let formatFile
          | isInfixOf "/" formatName || exists
          = formatName
          | otherwise
          = dataDir </> "data" </> "formats" </>
               (formatName ++ "_" ++ commandPropFormat opts)
    formatMissing <- not <$> doesFileExist formatFile

    if formatMissing
      then return $ Left $ commandIncorrectFormatSpec formatFile
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
                     (wrapper) (def) xmlFormat content
                     -- (fmap (fmap print) . wrapper) (print def) xmlFormat content
             | otherwise
             -> do let jsonFormat = read format
                   content <- B.safeReadFile fp
                   case content of
                     Left e  -> return $ Left e
                     Right b -> do case eitherDecode b of
                                     Left e  -> return $ Left e
                                     Right v ->
                                       parseJSONSpec
                                         (wrapper)
                                         jsonFormat
                                         v
        case res of
          Left e  -> return $ Left $ cannotOpenInputFile fp
          Right x -> return $ Right x

-- | Process a variable selection file, if available, and return the variable
-- names.
parseVariablesFile :: Maybe FilePath
                   -> ExceptT ErrorTriplet IO (Maybe [String])
parseVariablesFile Nothing   = return Nothing
parseVariablesFile (Just fp) = do
  -- Fail if the file cannot be opened.
  varNamesE <- liftIO $ E.try $ lines <$> readFile fp
  case (varNamesE :: Either E.SomeException [String]) of
    Left _         -> throwError $ cannotOpenVarFile fp
    Right varNames -> return $ Just varNames

-- | Process a requirements / handlers list file, if available, and return the
-- handler names.
parseRequirementsListFile :: Maybe FilePath
                          -> ExceptT ErrorTriplet IO (Maybe [String])
parseRequirementsListFile Nothing   = return Nothing
parseRequirementsListFile (Just fp) =
  ExceptT $ makeLeftE (cannotOpenHandlersFile fp) <$>
    (E.try $ Just . lines <$> readFile fp)

-- | Process a variable database file, if available, and return the rows in it.
parseVarDBFile :: Read a
               => Maybe FilePath
               -> ExceptT ErrorTriplet IO [a]
parseVarDBFile Nothing   = return []
parseVarDBFile (Just fn) =
  ExceptT $ makeLeftE (cannotOpenDB fn) <$>
    (E.try $ fmap read <$> lines <$> readFile fn)

-- | Check that the arguments provided are sufficient to operate.
--
-- The backend provides several modes of operation, which are selected
-- by providing different arguments to the `ros` command.
--
-- When an input specification file is provided, the variables and requirements
-- defined in it are used unless variables or handlers files are provided, in
-- which case the latter take priority.
--
-- If an input file is not provided, then the user must provide BOTH a variable
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

-- | Extract the variables from a specification, and sanitize them.
specExtractExternalVariables :: Maybe (Spec a) -> [String]
specExtractExternalVariables Nothing   = []
specExtractExternalVariables (Just cs) = map sanitizeLCIdentifier
                                       $ map externalVariableName
                                       $ externalVariables cs

-- | Extract the requirements from a specification, and sanitize them to match
-- the names of the handlers used by Copilot.
specExtractHandlers :: Maybe (Spec a) -> [String]
specExtractHandlers Nothing   = []
specExtractHandlers (Just cs) = map handlerNameF
                              $ map requirementName
                              $ requirements cs
  where
    handlerNameF = ("handler" ++) . sanitizeUCIdentifier

-- * Handler for boolean expressions

-- | Handler for boolean expressions that knows how to parse them, replace
-- variables in them, and convert them to Copilot.
--
-- It also contains a default value to be used whenever an expression cannot be
-- found in the input file.
data ExprPair = forall a . ExprPair
  { exprTPair   :: ExprPairT a
  }

data ExprPairT a = ExprPairT
  { exprTParse   :: String -> Either String a
  , exprTReplace :: [(String, String)] -> a -> a
  , exprTPrint   :: a -> String
  , exprTIdents  :: a -> [String]
  , exprTUnknown :: a
  }


-- | Return a handler depending on whether it should be for CoCoSpec boolean
-- expressions or for SMV boolean expressions. We default to SMV if not format
-- is given.
exprPair :: String -> ExprPair
exprPair "cocospec" = ExprPair $
  ExprPairT
    (CoCoSpec.pBoolSpec . CoCoSpec.myLexer)
    (\_ -> id)
    (CoCoSpec.boolSpec2Copilot)
    (CoCoSpec.boolSpecNames)
    (CoCoSpec.BoolSpecSignal (CoCoSpec.Ident "undefined"))
exprPair "literal" = ExprPair $
  ExprPairT
    Right
    (\_ -> id)
    id
    (const [])
    "undefined"
exprPair _ = ExprPair $
  ExprPairT
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

-- ** Error messages

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
cannotOpenInputFile :: FilePath -> ErrorTriplet
cannotOpenInputFile file =
    ErrorTriplet ecCannotOpenInputFile msg (LocationFile file)
  where
    msg =
      "cannot open input specification file " ++ file

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

-- | Error message associated to the format file not being found.
commandIncorrectFormatSpec :: FilePath -> ErrorTriplet
commandIncorrectFormatSpec formatFile =
    ErrorTriplet ecIncorrectFormatFile msg (LocationFile formatFile)
  where
    msg =
      "The format specification " ++ formatFile ++ " does not exist or is not "
      ++ "readable"

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

-- | Error: wrong arguments provided.
ecWrongArguments :: ErrorCode
ecWrongArguments = 1

-- | Error: the input specification provided by the user cannot be opened.
ecCannotOpenInputFile :: ErrorCode
ecCannotOpenInputFile = 1

-- | Error: the variable DB provided by the user cannot be opened.
ecCannotOpenDBFile :: ErrorCode
ecCannotOpenDBFile = 1

-- | Error: the variable file provided by the user cannot be opened.
ecCannotOpenVarFile :: ErrorCode
ecCannotOpenVarFile = 1

-- | Error: the handlers file provided by the user cannot be opened.
ecCannotOpenHandlersFile :: ErrorCode
ecCannotOpenHandlersFile = 1

-- | Error: the format file cannot be opened.
ecIncorrectFormatFile :: ErrorCode
ecIncorrectFormatFile = 1

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

-- | Replace the left Exception in an Either.
makeLeftE :: c -> Either E.SomeException b -> Either c b
makeLeftE c (Left _)   = Left c
makeLeftE _ (Right x)  = Right x
