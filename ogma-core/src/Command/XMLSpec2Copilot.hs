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
-- | Transform a XML file into a Copilot specification.
--
-- This module makes use of "Language.Trans.Spec2Copilot", which does most of
-- the work.
module Command.XMLSpec2Copilot
    ( xmlSpec2Copilot
    , XMLSpec2CopilotOptions(..)
    , ErrorCode
    )
  where

-- External imports
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IfElse (awhen)
import Data.Aeson           (decode, eitherDecode)
import Data.ByteString.Lazy (fromStrict)

-- External imports: auxiliary
import Data.ByteString.Extra as B ( safeReadFile )

-- Internal imports: auxiliary
import Command.Result ( Result (..) )
import Data.Location  ( Location (..) )

-- Internal imports: language ASTs, transformers
import Data.OgmaSpec (Spec)

import qualified Language.CoCoSpec.AbsCoCoSpec as CoCoSpec
import qualified Language.CoCoSpec.ParCoCoSpec as CoCoSpec ( myLexer,
                                                             pBoolSpec )

import Language.XMLSpec.Parser (XMLFormat (..), parseXMLSpec)

import qualified Language.SMV.AbsSMV       as SMV
import qualified Language.SMV.ParSMV       as SMV (myLexer, pBoolSpec)
import           Language.SMV.Substitution (substituteBoolExpr)

import qualified Language.Trans.CoCoSpec2Copilot as CoCoSpec (boolSpec2Copilot)
import           Language.Trans.SMV2Copilot      as SMV (boolSpec2Copilot)
import           Language.Trans.Spec2Copilot     (spec2Copilot, specAnalyze)

-- | Print the contents of a Copilot module that implements the Past-time TL
-- formula in an XML file.
--
-- PRE: The file given is readable, contains a valid XML file with a PT formula
-- in the @ptLTL@ subtag of each @requirement@ tag , the formula does not use
-- any identifiers that exist in Copilot, or any of @prop@, @clock@, @ftp@. All
-- identifiers used are valid C99 identifiers.
xmlSpec2Copilot :: FilePath -- ^ Path to a file containing an XML specification.
                -> XMLSpec2CopilotOptions
                            -- ^ Customization options formula
                -> IO (Result ErrorCode)
xmlSpec2Copilot fp options = do

  let functions = exprHandler (xmlSpec2CopilotUseCoCoSpec options)

  copilot <- xmlSpec2Copilot' fp options functions

  let (mOutput, result) =
        xmlSpec2CopilotResult options fp copilot

  awhen mOutput putStrLn
  return result

-- | Print the contents of a Copilot module that implements the Past-time TL
-- formula in an XML file, using a subexpression handler.
--
-- PRE: The file given is readable, contains a valid XML file with a PT formula
-- in the @ptLTL@ subtag of each @rquirement@ tag, the formula does not use any
-- identifiers that exist in Copilot, or any of @prop@, @clock@, @ftp@. All
-- identifiers used are valid C99 identifiers.
xmlSpec2Copilot' :: FilePath
                 -> XMLSpec2CopilotOptions
                 -> ExprHandler
                 -> IO (Either String String)
xmlSpec2Copilot' fp options (ExprHandler parse replace print) = do
  let name        = xmlSpec2CopilotFilename options
      useCoCoSpec = xmlSpec2CopilotUseCoCoSpec options
      typeMaps    = xmlTypeToCopilotTypeMapping options

      parse' = liftEither . parse

  -- All of the following operations use Either to return error messages. The
  -- use of the monadic bind to pass arguments from one function to the next
  -- will cause the program to stop at the earliest error.
  content <- B.safeReadFile fp
  res <- case content of
           Left s  -> return $ Left s
           Right b -> case eitherDecode b of
                        Left s  -> return $ Left s
                        Right x -> runExceptT $
                                     parseXMLSpec parse' (fretFormat useCoCoSpec) x

  let copilot = spec2Copilot name typeMaps replace print =<< specAnalyze =<< res

  return copilot

-- | Options used to customize the conversion of XML Specs to Copilot code.
data XMLSpec2CopilotOptions = XMLSpec2CopilotOptions
  { xmlSpec2CopilotUseCoCoSpec :: Bool
  , xmlSpec2CopilotIntType     :: String
  , xmlSpec2CopilotRealType    :: String
  , xmlSpec2CopilotFilename    :: String
  }

-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error code used is 1 for user error.
type ErrorCode = Int

-- | Error: the XML Spec file cannot be read due to the file being unreadable
-- or the format being incorrect.
ecFretCSError :: ErrorCode
ecFretCSError = 1

-- * Result

-- | Process the result of the transformation function.
xmlSpec2CopilotResult :: XMLSpec2CopilotOptions
                      -> FilePath
                      -> Either String String
                      -> (Maybe String, Result ErrorCode)
xmlSpec2CopilotResult options fp result = case result of
  Left msg -> (Nothing, Error ecFretCSError msg (LocationFile fp))
  Right t  -> (Just t, Success)

-- * Parser

-- | XMLPath selectors for a FRET file
fretFormat :: Bool -> XMLFormat
fretFormat = XMLFormat
  { specInternalVars    = Just "//Internal_variable"
  , specInternalVarId   = "/Internal_variable/name/text()"
  , specInternalVarExpr = "/Internal_variable/value/text()"
  , specInternalVarType = Nothing
  , specExternalVars    = Just "//Other_variable"
  , specExternalVarId   = "/Other_variable/name/text()"
  , specExternalVarType = Just "/Other_variable/type/text()"
  , specRequirements    = "//Requirement"
  , specRequirementId   = "/Requirement/name/text()"
  , specRequirementDesc = Nothing
  , specRequirementExpr = if useCoCoSpec
                            then "/Requirement/CoCoSpecCode/text()"
                            else "/Requirement/ptLTL/text()"
  }

-- * Mapping of types from FRET to Copilot
xmlTypeToCopilotTypeMapping :: XMLSpec2CopilotOptions
                            -> [(String, String)]
xmlTypeToCopilotTypeMapping options =
  [ ("bool",    "Bool")
  , ("int",     xmlSpec2CopilotIntType options)
  , ("integer", xmlSpec2CopilotIntType options)
  , ("real",    xmlSpec2CopilotRealType options)
  , ("string",  "String")
  ]

-- * Handler for boolean expressions

-- | Handler for boolean expressions that knows how to parse them, replace
-- variables in them, and convert them to Copilot.
data ExprHandler = forall a . ExprHandler
  { exprParse   :: String -> Either String a
  , exprReplace :: [(String, String)] -> a -> a
  , exprPrint   :: a -> String
  }

-- | Return a handler depending on whether it should be for CoCoSpec boolean
-- expressions or for SMV boolean expressions.
exprHandler :: Bool -> ExprHandler
exprHandler True  = ExprHandler (CoCoSpec.pBoolSpec . CoCoSpec.myLexer)
                                (\_ -> id)
                                (CoCoSpec.boolSpec2Copilot)
exprHandler False = ExprHandler (SMV.pBoolSpec . SMV.myLexer)
                                (substituteBoolExpr)
                                (SMV.boolSpec2Copilot)
