{-# LANGUAGE ExistentialQuantification #-}
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
import Data.Aeson           (decode, eitherDecode)
import Data.ByteString.Lazy (fromStrict, pack)
import Data.Foldable        (for_)
import Data.List            (nub, (\\))
import Data.Maybe           (fromMaybe)
import System.FilePath      ((</>))

-- External imports: auxiliary
import Data.ByteString.Extra as B ( safeReadFile )

-- Internal imports: auxiliary
import Command.Result  (Result (..))
import Data.Location   (Location (..))
import Paths_ogma_core (getDataDir)

-- Internal imports: language ASTs, transformers
import Data.OgmaSpec (ExternalVariableDef (..), InternalVariableDef (..),
                      Requirement (..), Spec (..))
import Language.JSONSpec.Parser (JSONFormat (..), parseJSONSpec)

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

-- | Print the contents of a Copilot module that implements the spec in an
-- input file.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @prop@, @clock@, @ftp@, @notPreviousNot@. All identifiers
-- used are valid C99 identifiers.
standalone :: FilePath          -- ^ Path to a file containing a specification
           -> StandaloneOptions -- ^ Customization options
           -> IO (Result ErrorCode)
standalone fp options = do

  let functions = exprPair (standalonePropFormat options)

  copilot <- standalone' fp options functions

  let (mOutput, result) = standaloneResult options fp copilot

  for_ mOutput putStrLn
  return result

-- | Print the contents of a Copilot module that implements the spec in an
-- input file, using a subexpression handler.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @prop@, @clock@, @ftp@, @notPreviousNot@. All identifiers
-- used are valid C99 identifiers.
standalone' :: FilePath
            -> StandaloneOptions
            -> ExprPair
            -> IO (Either String String)
standalone' fp options (ExprPair parse replace print ids) = do
  let name     = standaloneFilename options
      typeMaps = typeToCopilotTypeMapping options

  -- Obtain format file
  dataDir <- getDataDir
  let formatFile =
        dataDir </> "data" </> "formats" </>
          (standaloneFormat options ++ "_" ++ standalonePropFormat options)

  format <- read <$> readFile formatFile

  -- All of the following operations use Either to return error messages. The
  -- use of the monadic bind to pass arguments from one function to the next
  -- will cause the program to stop at the earliest error.
  content <- B.safeReadFile fp
  res <- case content of
           Left s  -> return $ Left s
           Right b -> return $ parseJSONSpec parse format =<< eitherDecode b

  -- Complement the specification with any missing/implicit definitions
  let res' = fmap (addMissingIdentifiers ids) res

  let copilot = spec2Copilot name typeMaps replace print =<< specAnalyze =<< res'

  return copilot

-- | Options used to customize the conversion of specifications to Copilot
-- code.
data StandaloneOptions = StandaloneOptions
  { standaloneFormat      :: String
  , standalonePropFormat  :: String
  , standaloneTypeMapping :: [(String, String)]
  , standaloneFilename    :: String
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

-- * Result

-- | Process the result of the transformation function.
standaloneResult :: StandaloneOptions
                 -> FilePath
                 -> Either String String
                 -> (Maybe String, Result ErrorCode)
standaloneResult options fp result = case result of
  Left msg -> (Nothing, Error ecStandaloneError msg (LocationFile fp))
  Right t  -> (Just t, Success)

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
data ExprPair = forall a . ExprPair
  { exprParse   :: String -> Either String a
  , exprReplace :: [(String, String)] -> a -> a
  , exprPrint   :: a -> String
  , exprIdents  :: a -> [String]
  }

-- | Return a handler depending on whether it should be for CoCoSpec boolean
-- expressions or for SMV boolean expressions. We default to SMV if not format
-- is given.
exprPair :: String -> ExprPair
exprPair "cocospec" = ExprPair (CoCoSpec.pBoolSpec . CoCoSpec.myLexer)
                               (\_ -> id)
                               (CoCoSpec.boolSpec2Copilot)
                               (CoCoSpec.boolSpecNames)
exprPair _          = ExprPair (SMV.pBoolSpec . SMV.myLexer)
                               (substituteBoolExpr)
                               (SMV.boolSpec2Copilot)
                               (SMV.boolSpecNames)

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
