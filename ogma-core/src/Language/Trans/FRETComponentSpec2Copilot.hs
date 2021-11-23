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
{-# LANGUAGE OverloadedStrings #-}

-- | Transform a FRET Component Specification into a Copilot specification.
--
-- Normally, this module would be implemented as a conversion between ASTs, but
-- we want to add comments to the generated code, which are not representable
-- in the abstract syntax tree.
module Language.Trans.FRETComponentSpec2Copilot where

-- External imports
import Data.List  ( intersect, union )
import Data.Maybe ( fromMaybe )

-- External imports: auxiliary
import Data.String.Extra ( sanitizeLCIdentifier, sanitizeUCIdentifier )

-- Internal imports: language ASTs, transformers
import qualified Language.CoCoSpec.ParCoCoSpec   as CoCoSpec ( myLexer,
                                                               pBoolSpec )
import           Language.FRETComponentSpec.AST  as FRET
import qualified Language.Trans.CoCoSpec2Copilot as CoCoSpec ( boolSpec2Copilot )
import           Language.Trans.SMV2Copilot      as SMV ( boolSpec2Copilot )

-- | Options used to customize the conversion of FRET Component Specifications
-- to Copilot code.
data FRETComponentSpec2CopilotOptions = FRETComponentSpec2CopilotOptions
  { fretCS2CopilotUseCoCoSpec :: Bool
  , fretCS2CopilotIntType     :: String
  , fretCS2CopilotRealType    :: String
  }

-- | Transform a FRET TL specification into a Copilot specification.
--
-- This function may fail with a 'Left' value if the resulting Copilot
-- specification would contain name clashes or other errors.
fretComponentSpec2Copilot :: FRETComponentSpec2CopilotOptions
                          -> FRETComponentSpec
                          -> Either String String
fretComponentSpec2Copilot prefs parseResult =
  fretComponentSpec2Copilot' prefs =<< fret2CopilotAnalyze parseResult

-- | For a given FRET file, return the corresponding Copilot file, or an error
-- message if such file cannot be generated.
--
-- PRE: there are no name clashes between the variables and names used in the
-- FRET specification and any definitions in Haskell's Prelude or in Copilot.
fretComponentSpec2Copilot' :: FRETComponentSpec2CopilotOptions
                           -> FRETComponentSpec
                           -> Either String String
fretComponentSpec2Copilot' prefs fretComponentSpec =
    unlines . concat <$> sequence
      [ pure imports
      , pure externs
      , internals
      , reqs
      , pure clock
      , pure ftp
      , pure pre
      , pure spec
      , pure main'
      ]

  where

    -- Import header block
    imports :: [String]
    imports =
      [ "import           Copilot.Compile.C99"
      , "import           Copilot.Language          hiding (prop)"
      , "import           Copilot.Language.Prelude"
      , "import           Copilot.Library.LTL       (next)"
      , "import           Copilot.Library.MTL       hiding (since,"
        ++ " alwaysBeen, trigger)"
      , "import           Copilot.Library.PTLTL     (since, previous,"
        ++ " alwaysBeen)"
      , "import qualified Copilot.Library.PTLTL     as PTLTL"
      , "import qualified Copilot.Library.MTL       as MTL"
      , "import           Language.Copilot          (reify)"
      , "import           Prelude                   hiding ((&&), (||), (++),"
        ++ " (<=), (>=), (<), (>), (==), not)"
      , ""
      ]

    -- Extern streams
    externs = concatMap externVarToDecl
                        (FRET.fretExternalVariables fretComponentSpec)
      where
        externVarToDecl i = [ FRET.fretExternalVariableName i
                                ++ " :: Stream "
                                ++ "("
                                ++ fretTypeToCopilotType
                                     prefs
                                     (FRET.fretExternalVariableType i)
                                ++ ")"

                            , FRET.fretExternalVariableName i
                                ++ " = "
                                ++ "extern"
                                ++ " "
                                ++ show (FRET.fretExternalVariableName i)
                                ++ " "
                                ++ "Nothing"
                            , ""
                            ]

    -- Internal stream definitions
    internals = concat
             <$> mapM internalVarToDecl
                      (FRET.fretInternalVariables fretComponentSpec)
      where
        internalVarToDecl i = fmap (\implem ->
                                [ FRET.fretInternalVariableName i
                                    ++ " :: Stream "
                                    ++ "("
                                    ++ fretTypeToCopilotType
                                         prefs
                                         (FRET.fretInternalVariableType i)
                                    ++ ")"

                                , FRET.fretInternalVariableName i
                                    ++ " = "
                                    ++ implem

                                , ""
                                ]) implementation
          where
            implementation = if null (FRET.fretInternalVariableCopilot i)
                               then CoCoSpec.boolSpec2Copilot
                                      <$> CoCoSpec.pBoolSpec
                                            ( CoCoSpec.myLexer
                                            $ FRET.fretInternalVariableLustre i
                                            )
                               else pure (FRET.fretInternalVariableCopilot i)

    -- Encoding of requirements as boolean streams
    reqs :: Either String [String]
    reqs = concat <$> mapM reqToDecl (FRET.fretRequirements fretComponentSpec)
      where
        reqToDecl i = sequence
                        [ pure reqComment, pure reqSignature, reqBody, pure "" ]
          where
            -- Definition comment, which includes the requirement for
            -- traceability purposes.
            reqComment = "-- | "  ++ FRET.fretRequirementName i    ++ "\n" ++
                         "--   @"                                  ++ "\n" ++
                         "--   "  ++ FRET.fretRequirementFretish i ++ "\n" ++
                         "--   @"

            -- Definition type signature.
            reqSignature = FRET.fretRequirementName i
                             ++ " :: " ++ "Stream" ++ " " ++ "Bool"

            -- Definition implementation, either in SMV or in CoCoSpec
            reqBody = if fretCS2CopilotUseCoCoSpec prefs
                        then reqBodyCoCo
                        else reqBodyPT

            reqBodyPT = fmap (\e -> FRET.fretRequirementName i ++ " = "
                                      ++ SMV.boolSpec2Copilot e
                             )
                             (fromMaybe (Left $ "No requirement for " ++ show i)
                                        (FRET.fretRequirementPTExpanded i))

            reqBodyCoCo = fmap
                            (\e -> FRET.fretRequirementName i ++ " = "
                                     ++ CoCoSpec.boolSpec2Copilot e
                            )
                            (fromMaybe (Left $ "No requirement for " ++ show i)
                                       (FRET.fretRequirementCoCoSpec i))

    -- Auxiliary streams: clock
    clock :: [String]
    clock = [ ""
            , "-- | Clock that increases in one-unit steps."
            , "clock :: Stream Int64"
            , "clock = [0] ++ (clock + 1)"
            , ""
            ]

    -- Auxiliary streams: first time point
    ftp :: [String]
    ftp = [ ""
          , "-- | First Time Point"
          , "ftp :: Stream Bool"
          , "ftp = [True] ++ false"
          , ""
          ]

    -- Auxiliary streams: pre
    pre = [ ""
          , "pre :: Stream Bool -> Stream Bool"
          , "pre = ([False] ++)"
          ]

    -- Main specification
    spec :: [String]
    spec = [ ""
           , "-- | Complete specification. Calls the C function void "
             ++ " handler(); when"
           , "-- the property is violated."
           , "spec :: Spec"
           , "spec = do"
           ]
           ++ triggers
           ++ [ "" ]
      where
        triggers :: [String]
        triggers = fmap reqTrigger (FRET.fretRequirements fretComponentSpec)

        reqTrigger :: FRETRequirement -> String
        reqTrigger r = "  trigger " ++ show handlerName ++ " (not "
                       ++ propName ++ ") " ++ "[]"
          where
            handlerName = "handler" ++ FRET.fretRequirementName r
            propName    = FRET.fretRequirementName r

    -- Main program that compiles specification to C in two files (code and
    -- header).
    main' :: [String]
    main' = [ ""
            , "main :: IO ()"
            , "main = reify spec >>= compile \"fret\""
            ]

-- | Return the corresponding type in Copilot matching a given FRET type.
fretTypeToCopilotType :: FRETComponentSpec2CopilotOptions -> String -> String
fretTypeToCopilotType _options "bool"    = "Bool"
fretTypeToCopilotType options  "int"     = fretCS2CopilotIntType options
fretTypeToCopilotType options  "integer" = fretCS2CopilotIntType options
fretTypeToCopilotType options  "real"    = fretCS2CopilotRealType options
fretTypeToCopilotType _options "string"  = "String"
fretTypeToCopilotType _options x         = x

-- | Analyze a FRET-Copilot file and determine if there will be any name
-- clashes after the conversion to Copilot.
--
-- This function does not compare against Haskell's prelude or Copilot's
-- modules. It simply makes simple conversions to comply with Copilot/Haskell's
-- grammar (e.g., variable/function names start with lowercase) and determines
-- if the conversion would make two definitions in the given specification
-- produce name clashes between them.
fret2CopilotAnalyze :: FRETComponentSpec -> Either String FRETComponentSpec
fret2CopilotAnalyze fretComponentSpec
    | not (null evnClash)
    = Left $ "Name clash detected: " ++ show evnClash

    | not (null ivnClash)
    = Left $ "Name clash detected: " ++ show ivnClash

    | not (null reqClash)
    = Left $ "Name clash detected: " ++ show reqClash

    | otherwise
    = Right $ foldr applySubstitution fretComponentSpec nameSubstitutions

  where

    -- Sets containing name clashes
    ivnClash = internalVariableNames'
                 `intersect` (externalVariableNames' `union` requirementNames')

    evnClash = externalVariableNames'
                 `intersect` (internalVariableNames' `union` requirementNames')

    reqClash = requirementNames'
                 `intersect` (internalVariableNames'
                                `union` externalVariableNames')

    -- Names used.
    internalVariableNames' = map snd internalVariableMap
    externalVariableNames' = map snd externalVariableMap
    requirementNames'      = map snd requirementNameMap

    -- Map from a variable name to its desired identifier in the code
    -- generated.
    internalVariableMap =
      map (\x -> (x, sanitizeLCIdentifier x)) internalVariableNames

    externalVariableMap =
      map (\x -> (x, sanitizeLCIdentifier x)) externalVariableNames

    requirementNameMap =
      map (\x -> (x, "prop" ++ sanitizeUCIdentifier x)) requirementNames

    nameSubstitutions = internalVariableMap
                     ++ externalVariableMap
                     ++ requirementNameMap

    -- Variable/requirement names used in the component spec.
    internalVariableNames = map fretInternalVariableName
                          $ fretInternalVariables fretComponentSpec

    externalVariableNames = map fretExternalVariableName
                          $ fretExternalVariables fretComponentSpec

    requirementNames = map fretRequirementName
                     $ fretRequirements fretComponentSpec
