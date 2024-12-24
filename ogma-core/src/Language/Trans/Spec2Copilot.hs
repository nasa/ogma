-- Copyright 2024 United States Government as represented by the Administrator
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
import Data.List  ( intercalate, intersect, lookup, union )
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
             -> Either String (String, String, String, String, String)
spec2Copilot specName typeMaps exprTransform showExpr spec =
    pure (externs, internals, reqs, triggers, specName)

  where

    -- Extern streams
    externs = unlines'
            $ intercalate [""]
            $ map externVarToDecl
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
                            ]
          where
            propName = safeMap nameSubstitutions (externalVariableName i)

    -- Internal stream definitions
    internals = unlines'
              $ intercalate [""]
              $ map internalVarToDecl
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
                                ]) implementation
          where
            propName = safeMap nameSubstitutions (internalVariableName i)
            implementation = (internalVariableExpr i)

    -- Encoding of requirements as boolean streams
    reqs :: String
    reqs = unlines'
         $ intercalate [""]
         $ map reqToDecl (requirements spec)
      where
        reqToDecl i = [ reqComment, reqSignature
                      , reqBody nameSubstitutions
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


    -- Main specification triggers
    triggers :: String
    triggers = unlines' $ fmap reqTrigger (requirements spec)
      where
        reqTrigger :: Requirement a -> String
        reqTrigger r = "  trigger " ++ show handlerName ++ " (not "
                       ++ propName ++ ") " ++ "[]"
          where
            handlerName = "handler" ++ sanitizeUCIdentifier (requirementName r)
            propName    = safeMap nameSubstitutions (requirementName r)

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

-- | Create a string from a list of strings, inserting new line characters
-- between them. Unlike 'Prelude.unlines', this function does not insert
-- an end of line character at the end of the last string.
unlines' :: [String] -> String
unlines' = intercalate "\n"
