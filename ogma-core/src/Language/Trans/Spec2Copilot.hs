{-# LANGUAGE ScopedTypeVariables #-}
-- Copyright 2024 United States Government as represented by the Administrator
-- Copyright 2024 United States Government as represented by the Administrator
-- of the National Aeronautics and Space Administration. All Rights Reserved.
--
-- Disclaimers
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at
--
--      https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.

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
spec2Copilot :: forall a
             .  String                         -- Spec / target file name
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
                       ++ propName ++ ") " ++ handlerArg
          where
            handlerName = "handler" ++ sanitizeUCIdentifier (requirementName r)
            propName    = safeMap nameSubstitutions (requirementName r)
            handlerArg  =
              case (requirementResultType r, requirementResultExpr r) of
                (Just ty, Just ex) -> "[ arg (" ++ showExpr ex ++ " ) ]"
                _                  -> "[]"

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

    -- Variable/requirement names used in the input spec.
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

    -- Variable/requirement names used in the input spec.
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
