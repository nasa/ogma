-- Copyright 2024 United States Government as represented by the Administrator
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

-- | Transform an Ogma specification into a standalone Copilot specification.
--
-- Normally, this module would be implemented as a conversion between ASTs, but
-- we want to add comments to the generated code, which are not representable
-- in the abstract syntax tree.
module Language.Trans.Spec2Copilot where

-- External imports
import Data.List  ( intersect, lookup, union )
import Data.Maybe ( fromMaybe )

-- External imports: auxiliary
import Data.String.Extra ( sanitizeLCIdentifier, sanitizeUCIdentifier )

-- External imports: ogma-spec
import Data.OgmaSpec (ExternalVariableDef (..), InternalVariableDef (..),
                      Requirement (..), Spec (..))

-- | For a given spec, return the corresponding Copilot file, or an error
-- message if such file cannot be generated.
--
-- PRE: there are no name clashes between the variables and names used in the
-- specification and any definitions in Haskell's Prelude or in Copilot.
spec2Copilot :: String                         -- Spec / target file name
             -> [(String, String)]             -- Type substitution table
             -> ([(String, String)] -> a -> a) -- Expr subsitution function
             -> (a -> String)                  -- Expr show function
             -> Spec a                         -- Specification
             -> Either String String
spec2Copilot specName typeMaps exprTransform showExpr spec =
    pure $ unlines $ concat
      [ imports
      , externs
      , internals
      , reqs
      , clock
      , ftp
      , pre
      , tpre
      , notPreviousNot
      , copilotSpec
      , main'
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
        ++ " (<=), (>=), (<), (>), (==), (/=), not)"
      , ""
      ]

    -- Extern streams
    externs = concatMap externVarToDecl
                        (externalVariables spec)
      where
        externVarToDecl i = [ propName
                                ++ " :: Stream "
                                ++ "("
                                ++ safeMap typeMaps (externalVariableType i)
                                ++ ")"

                            , propName
                                ++ " = "
                                ++ "extern"
                                ++ " "
                                ++ show (externalVariableName i)
                                ++ " "
                                ++ "Nothing"
                            , ""
                            ]
          where
            propName = safeMap nameSubstitutions (externalVariableName i)

    -- Internal stream definitions
    internals = concatMap internalVarToDecl
                      (internalVariables spec)
      where
        internalVarToDecl i = (\implem ->
                                [ propName
                                    ++ " :: Stream "
                                    ++ "("
                                    ++ safeMap typeMaps (internalVariableType i)
                                    ++ ")"

                                , propName
                                    ++ " = "
                                    ++ implem

                                , ""
                                ]) implementation
          where
            propName = safeMap nameSubstitutions (internalVariableName i)
            implementation = (internalVariableExpr i)

    -- Encoding of requirements as boolean streams
    reqs :: [String]
    reqs = concatMap reqToDecl (requirements spec)
      where
        reqToDecl i = [ reqComment, reqSignature
                      , reqBody nameSubstitutions
                      , ""
                      ]
          where
            reqName = safeMap nameSubstitutions (requirementName i)

            -- Definition comment, which includes the requirement for
            -- traceability purposes.
            reqComment = "-- | "  ++ requirementName i    ++ "\n" ++
                         "--   @"                                  ++ "\n" ++
                         "--   "  ++ requirementDescription i ++ "\n" ++
                         "--   @"

            -- Definition type signature.
            reqSignature = reqName
                             ++ " :: " ++ "Stream" ++ " " ++ "Bool"

            -- Definition implementation. We use an auxiliary function to
            -- transform the implementation into Copilot, applying a
            -- substitution.
            reqBody subs = reqName ++ " = " ++
                             (showExpr (exprTransform subs (requirementExpr i)))


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

    -- Auxiliary streams: tpre
    tpre = [ ""
           , "tpre :: Stream Bool -> Stream Bool"
           , "tpre = ([True] ++)"
           ]

    -- Auxiliary streams: notPreviousNot
    notPreviousNot :: [String]
    notPreviousNot = [ ""
                     , "notPreviousNot :: Stream Bool -> Stream Bool"
                     , "notPreviousNot = not . PTLTL.previous . not"
                     ]

    -- Main specification
    copilotSpec :: [String]
    copilotSpec = [ ""
                  , "-- | Complete specification. Calls C handler functions"
                    ++ " when"
                  , "-- properties are violated."
                  , "spec :: Spec"
                  , "spec = do"
                  ]
                  ++ triggers
                  ++ [ "" ]
      where
        triggers :: [String]
        triggers = fmap reqTrigger (requirements spec)

        reqTrigger :: Requirement a -> String
        reqTrigger r = "  trigger " ++ show handlerName ++ " (not "
                       ++ propName ++ ") " ++ "[]"
          where
            handlerName = "handler" ++ sanitizeUCIdentifier (requirementName r)
            propName    = safeMap nameSubstitutions (requirementName r)

    -- Main program that compiles specification to C in two files (code and
    -- header).
    main' :: [String]
    main' = [ ""
            , "main :: IO ()"
            , "main = reify spec >>= compile \"" ++ specName ++ "\""
            ]

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
    internalVariableNames = map internalVariableName
                          $ internalVariables spec

    externalVariableNames = map externalVariableName
                          $ externalVariables spec

    requirementNames = map requirementName
                     $ requirements spec

specAnalyze :: Spec a -> Either String (Spec a)
specAnalyze spec
    | not (null evnClash)
    = Left $ "Name clash detected: " ++ show evnClash

    | not (null ivnClash)
    = Left $ "Name clash detected: " ++ show ivnClash

    | not (null reqClash)
    = Left $ "Name clash detected: " ++ show reqClash

    | otherwise
    = Right spec

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

    -- Variable/requirement names used in the component spec.
    internalVariableNames = map internalVariableName
                          $ internalVariables spec

    externalVariableNames = map externalVariableName
                          $ externalVariables spec

    requirementNames = map requirementName
                     $ requirements spec

-- * Auxiliary

-- | Substitute a string based on a given substitution table.
--
-- This function leaves the key unchanged if it cannot be found in the
-- substitution table.
safeMap :: [(String, String)] -> String -> String
safeMap ls k = fromMaybe k $ lookup k ls
