{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
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
-- | Transform a specification into a standalone Copilot specification.
module Command.Standalone
    ( standalone
    , StandaloneOptions(..)
    , ErrorCode
    )
  where

-- External imports
import Control.Exception    as E
import Data.Aeson           (decode, eitherDecode, object, (.=))
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable        (for_)
import Data.List            (isInfixOf, isPrefixOf, nub, (\\))
import Data.Maybe           (fromMaybe)
import System.Directory     (doesFileExist)
import System.Process       (readProcess)
import System.FilePath      ((</>))
import Data.Text.Lazy       (pack)

-- External imports: auxiliary
import Data.ByteString.Extra  as B ( safeReadFile )
import System.Directory.Extra ( copyTemplate )

-- Internal imports: auxiliary
import Command.Result  (Result (..))
import Data.Location   (Location (..))
import Paths_ogma_core (getDataDir)

-- Internal imports: language ASTs, transformers
import Data.OgmaSpec (ExternalVariableDef (..), InternalVariableDef (..),
                      Requirement (..), Spec (..))
import Language.JSONSpec.Parser (JSONFormat (..), parseJSONSpec)
import Language.XMLSpec.Parser  (parseXMLSpec)

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
import           Language.Trans.Spec2Copilot     (spec2Copilot, specAnalyze)

-- | Generate a new standalone Copilot monitor that implements the spec in an
-- input file.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @prop@, @clock@, @ftp@, @notPreviousNot@. All identifiers
-- used are valid C99 identifiers. The template, if provided, exists and uses
-- the variables needed by the standalone application generator. The target
-- directory is writable and there's enough disk space to copy the files over.
standalone :: FilePath          -- ^ Path to a file containing a specification
           -> StandaloneOptions -- ^ Customization options
           -> IO (Result ErrorCode)
standalone fp options = do
  E.handle (return . standaloneTemplateError options fp) $ do
    -- Obtain template dir
    templateDir <- case standaloneTemplateDir options of
                     Just x  -> return x
                     Nothing -> do
                       dataDir <- getDataDir
                       return $ dataDir </> "templates" </> "standalone"

    let functions = exprPair (standalonePropFormat options)

    copilot <- standalone' fp options functions

    let (mOutput, result) = standaloneResult options fp copilot

    for_ mOutput $ \(externs, internals, reqs, triggers, specName) -> do
      let subst = object $
                    [ "externs"   .= pack externs
                    , "internals" .= pack internals
                    , "reqs"      .= pack reqs
                    , "triggers"  .= pack triggers
                    , "specName"  .= pack specName
                    ]

      let targetDir = standaloneTargetDir options

      copyTemplate templateDir subst targetDir

    return result

-- | Generate a new standalone Copilot monitor that implements the spec in an
-- input file, using a subexpression handler.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @prop@, @clock@, @ftp@, @notPreviousNot@. All identifiers
-- used are valid C99 identifiers. The template, if provided, exists and uses
-- the variables needed by the standalone application generator. The target
-- directory is writable and there's enough disk space to copy the files over.
standalone' :: FilePath
            -> StandaloneOptions
            -> ExprPair
            -> IO (Either String (String, String, String, String, String))
standalone' fp options (ExprPair parse replace print ids def) = do
  let name     = standaloneFilename options
      typeMaps = typeToCopilotTypeMapping options

  -- Obtain format file.
  --
  -- A format name that exists as a file in the disk always takes preference
  -- over a file format included with Ogma. A file format with a forward slash
  -- in the name is always assumed to be a user-provided filename.
  -- Regardless of whether the file is user-provided or known to Ogma, we check
  -- (again) whether the file exists, and print an error message if not.
  let formatName = standaloneFormat options
  exists  <- doesFileExist formatName
  dataDir <- getDataDir
  let formatFile
        | isInfixOf "/" formatName || exists
        = formatName
        | otherwise
        = dataDir </> "data" </> "formats" </>
             (standaloneFormat options ++ "_" ++ standalonePropFormat options)
  formatMissing <- not <$> doesFileExist formatFile

  if formatMissing
    then return $ Left $ standaloneIncorrectFormatSpec formatFile
    else do
      format <- readFile formatFile

      let wrapper = wrapVia (standalonePropVia options) parse
      -- All of the following operations use Either to return error messages.
      -- The use of the monadic bind to pass arguments from one function to the
      -- next will cause the program to stop at the earliest error.
      res <-
        if | isPrefixOf "XMLFormat" format
           -> do let xmlFormat = read format
                 content <- readFile fp
                 parseXMLSpec wrapper def xmlFormat content
           | otherwise
           -> do let jsonFormat = read format
                 content <- B.safeReadFile fp
                 case content of
                   Left s  -> return $ Left s
                   Right b -> do case eitherDecode b of
                                   Left e  -> return $ Left e
                                   Right v -> parseJSONSpec wrapper jsonFormat v

      -- Complement the specification with any missing/implicit definitions
      let res' = fmap (addMissingIdentifiers ids) res

      return $ spec2Copilot name typeMaps replace print =<< specAnalyze =<< res'

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
  E.handle (\(e :: IOException) -> return $ Left $ show e) $ do
    out <- readProcess f [] s
    return $ parse out

-- | Options used to customize the conversion of specifications to Copilot
-- code.
data StandaloneOptions = StandaloneOptions
  { standaloneTargetDir   :: FilePath
  , standaloneTemplateDir :: Maybe FilePath
  , standaloneFormat      :: String
  , standalonePropFormat  :: String
  , standaloneTypeMapping :: [(String, String)]
  , standaloneFilename    :: String
  , standalonePropVia     :: Maybe String
  }

-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error code used is 1 for user error.
type ErrorCode = Int

-- | Error: the input file cannot be read due to it being unreadable or the
-- format being incorrect.
ecStandaloneError :: ErrorCode
ecStandaloneError = 1

-- | Error: standalone component generation failed during the copy/write
-- process.
ecStandaloneTemplateError :: ErrorCode
ecStandaloneTemplateError = 2

-- * Result

-- | Process the result of the transformation function.
standaloneResult :: StandaloneOptions
                 -> FilePath
                 -> Either String a
                 -> (Maybe a, Result ErrorCode)
standaloneResult options fp result = case result of
  Left msg -> (Nothing, Error ecStandaloneError msg (LocationFile fp))
  Right t  -> (Just t, Success)

-- | Report an error when trying to open or copy the template
standaloneTemplateError :: StandaloneOptions
                        -> FilePath
                        -> E.SomeException
                        -> Result ErrorCode
standaloneTemplateError options fp exception =
    Error ecStandaloneTemplateError msg (LocationFile fp)
  where
    msg =
      "Standlone monitor generation failed during copy/write operation. Check"
      ++ " that there's free space in the disk and that you have the necessary"
      ++ " permissions to write in the destination directory. "
      ++ show exception

-- | Error message associated to the format file not being found.
standaloneIncorrectFormatSpec :: String -> String
standaloneIncorrectFormatSpec formatFile =
  "The format specification " ++ formatFile ++ " does not exist or is not "
  ++ "readable"

-- * Mapping of types from input format to Copilot
typeToCopilotTypeMapping :: StandaloneOptions -> [(String, String)]
typeToCopilotTypeMapping options =
    [ ("bool",    "Bool")
    , ("int",     intType)
    , ("integer", intType)
    , ("real",    realType)
    , ("string",  "String")
    , ("",        "_")
    ]
  where
    intType  = fromMaybe "Int64" $ lookup "int" types
    realType = fromMaybe "Float" $ lookup "real" types

    types = standaloneTypeMapping options

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
exprPair "cocospec" = ExprPair (CoCoSpec.pBoolSpec . CoCoSpec.myLexer)
                               (\_ -> id)
                               (CoCoSpec.boolSpec2Copilot)
                               (CoCoSpec.boolSpecNames)
                               (CoCoSpec.BoolSpecSignal (CoCoSpec.Ident "undefined"))
exprPair _          = ExprPair (SMV.pBoolSpec . SMV.myLexer)
                               (substituteBoolExpr)
                               (SMV.boolSpec2Copilot)
                               (SMV.boolSpecNames)
                               (SMV.BoolSpecSignal (SMV.Ident "undefined"))

-- | Add to a spec external variables for all identifiers mentioned in
-- expressions that are not defined anywhere.
addMissingIdentifiers :: (a -> [String]) -> Spec a -> Spec a
addMissingIdentifiers f s = s { externalVariables = vars' }
  where
    vars'   = externalVariables s ++ newVars
    newVars = map (\n -> ExternalVariableDef n "") newVarNames

    -- Names that are not defined anywhere
    newVarNames = identifiers \\ existingNames

    -- Identifiers being mentioned in the requirements.
    identifiers = nub $ concatMap (f . requirementExpr) (requirements s)

    -- Names that are defined in variables.
    existingNames = map externalVariableName (externalVariables s)
                 ++ map internalVariableName (internalVariables s)
