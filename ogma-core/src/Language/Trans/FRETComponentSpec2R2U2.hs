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

-- | Transform a FRET Component Specification into a R2U2 specification.
--
-- Normally, this module would be implemented as a conversion between ASTs, but
-- we want to add comments to the generated code, which are not representable
-- in the abstract syntax tree.
module Language.Trans.FRETComponentSpec2R2U2 where

-- External imports
import Data.List  ( intersect, union )
import Data.Maybe ( fromMaybe )

-- External imports: auxiliary
import Data.String.Extra ( sanitizeLCIdentifier, sanitizeUCIdentifier )

-- Internal imports: language ASTs, transformers
import qualified Language.CoCoSpec.ParCoCoSpec   as CoCoSpec ( myLexer,
                                                               pBoolSpec )
import           Language.FRETComponentSpec.AST  as FRET
import qualified Language.Trans.CoCoSpec2R2U2 as CoCoSpec ( boolSpec2R2U2 )
import           Language.Trans.SMV2R2U2      as SMV ( boolSpec2R2U2 )

-- | Options used to customize the conversion of FRET Component Specifications
-- to R2U2 code.
data FRETComponentSpec2R2U2Options = FRETComponentSpec2R2U2Options
  { fretCS2R2U2UseCoCoSpec :: Bool
  , fretCS2R2U2IntType     :: String
  , fretCS2R2U2RealType    :: String
  }

-- | Transform a FRET TL specification into a R2U2 specification.
--
-- This function may fail with a 'Left' value if the resulting R2U2
-- specification would contain name clashes or other errors.
fretComponentSpec2R2U2 :: FRETComponentSpec2R2U2Options
                          -> FRETComponentSpec
                          -> Either String String
fretComponentSpec2R2U2 prefs parseResult =
  fretComponentSpec2R2U2' prefs =<< fret2R2U2Analyze parseResult

-- | For a given FRET file, return the corresponding R2U2 file, or an error
-- message if such file cannot be generated.
--
-- PRE: there are no name clashes between the variables and names used in the
-- FRET specification and any definitions in Haskell's Prelude or in R2U2.
fretComponentSpec2R2U2' :: FRETComponentSpec2R2U2Options
                           -> FRETComponentSpec
                           -> Either String String
fretComponentSpec2R2U2' prefs fretComponentSpec =
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
      [ "import           R2U2.Compile.C99"
      , "import           R2U2.Language          hiding (prop)"
      , "import           R2U2.Language.Prelude"
      , "import           R2U2.Library.LTL       (next)"
      , "import           R2U2.Library.MTL       hiding (since,"
        ++ " alwaysBeen, trigger)"
      , "import           R2U2.Library.PTLTL     (since, previous,"
        ++ " alwaysBeen)"
      , "import qualified R2U2.Library.PTLTL     as PTLTL"
      , "import qualified R2U2.Library.MTL       as MTL"
      , "import           Language.R2U2          (reify)"
      , "import           Prelude                   hiding ((&&), (||), (++),"
        ++ " (<=), (>=), (<), (>), (==), (/=), not)"
      , ""
      ]

    -- Extern streams
    externs = concatMap externVarToDecl
                        (FRET.fretExternalVariables fretComponentSpec)
      where
        externVarToDecl i = [ FRET.fretExternalVariableName i
                                ++ " :: Stream "
                                ++ "("
                                ++ fretTypeToR2U2Type
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
                                    ++ fretTypeToR2U2Type
                                         prefs
                                         (FRET.fretInternalVariableType i)
                                    ++ ")"

                                , FRET.fretInternalVariableName i
                                    ++ " = "
                                    ++ implem

                                , ""
                                ]) implementation
          where
            implementation = if null (FRET.fretInternalVariableR2U2 i)
                               then CoCoSpec.boolSpec2R2U2
                                      <$> CoCoSpec.pBoolSpec
                                            ( CoCoSpec.myLexer
                                            $ FRET.fretInternalVariableLustre i
                                            )
                               else pure (FRET.fretInternalVariableR2U2 i)

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
            reqBody = if fretCS2R2U2UseCoCoSpec prefs
                        then reqBodyCoCo
                        else reqBodyPT

            reqBodyPT = fmap (\e -> FRET.fretRequirementName i ++ " = "
                                      ++ SMV.boolSpec2R2U2 e
                             )
                             (fromMaybe (Left $ "No requirement for " ++ show i)
                                        (FRET.fretRequirementPTExpanded i))

            reqBodyCoCo = fmap
                            (\e -> FRET.fretRequirementName i ++ " = "
                                     ++ CoCoSpec.boolSpec2R2U2 e
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

-- | Return the corresponding type in R2U2 matching a given FRET type.
fretTypeToR2U2Type :: FRETComponentSpec2R2U2Options -> String -> String
fretTypeToR2U2Type _options "bool"    = "Bool"
fretTypeToR2U2Type options  "int"     = fretCS2R2U2IntType options
fretTypeToR2U2Type options  "integer" = fretCS2R2U2IntType options
fretTypeToR2U2Type options  "real"    = fretCS2R2U2RealType options
fretTypeToR2U2Type _options "string"  = "String"
fretTypeToR2U2Type _options x         = x

-- | Analyze a FRET-R2U2 file and determine if there will be any name
-- clashes after the conversion to R2U2.
--
-- This function does not compare against Haskell's prelude or R2U2's
-- modules. It simply makes simple conversions to comply with R2U2/Haskell's
-- grammar (e.g., variable/function names start with lowercase) and determines
-- if the conversion would make two definitions in the given specification
-- produce name clashes between them.
fret2R2U2Analyze :: FRETComponentSpec -> Either String FRETComponentSpec
fret2R2U2Analyze fretComponentSpec
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
