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

-- | Formally analyze specifications and provide information about them.
module Data.Spec.Analysis
    ( AnalysisResult(..)
    , specAnalyze
    )
  where

-- External imports
import qualified Copilot.Core as Core
import           Data.List    (intercalate, lookup)
import           Data.Maybe   (fromMaybe)

-- External imports: auxiliary
import Data.String.Extra (sanitizeLCIdentifier, sanitizeUCIdentifier)

-- External imports: ogma-spec
import Data.OgmaSpec (ExternalVariableDef (..), InternalVariableDef (..),
                      Requirement (..), Spec (..))

-- Internal imports
import Copilot.Core.Analysis        (exprIsConstant)
import Copilot.Language.Reify.Extra (reifySpec)

-- * Analysis of Specs

-- | Result of analyzing a specification.
data AnalysisResult = AnalysisResult
  { numAlwaysTrue  :: Int      -- ^ Number of always true requirements.
  , numAlwaysFalse :: Int      -- ^ Number of always false requirements.
  , alwaysTrueReq  :: [String] -- ^ List of always true requirements.
  , alwaysFalseReq :: [String] -- ^ List of always false requirements.
  , consistent     :: Bool     -- ^ Whether requirements are mutually
                               -- consistent.
  }

-- | Formally analyze a specification for redundancies, conflicts, etc.
specAnalyze :: [(String, String)]             -- Type substitution table
            -> ([(String, String)] -> a -> a) -- Expr substitution function
            -> (a -> String)                  -- Expr show function
            -> Spec a                         -- Specification
            -> IO (Either String AnalysisResult)
specAnalyze typeMaps exprTransform showExpr spec = do
  let structuredSpec =
        spec2Copilot typeMaps exprTransform showExpr spec

  coreSpec <- reifySpec defaultSpecImports $ showSpec structuredSpec

  let properties     = zip propertyNames propertyGuards
      propertyNames  = map (\(_, p, _, _, _) -> p)
                     $ copilotProperties structuredSpec
      propertyGuards = map Core.triggerGuard $ Core.specTriggers coreSpec

  constantProperties <- mapM (uncurry $ exprIsConstant coreSpec) properties

  let constantProperties' = zip propertyNames constantProperties

  let numTrue  = length $ filter fst constantProperties
      numFalse = length $ filter snd constantProperties

      trueReqs  = map fst $ filter (fst . snd) constantProperties'
      falseReqs = map fst $ filter (snd . snd) constantProperties'

  let negatedConjunction = Core.Op1 Core.Not
                         $ foldr (Core.Op2 Core.And) true propertyGuards
      true               = Core.Const Core.Bool True

  provedNegatedConjunction <-
    exprIsConstant coreSpec "ogma_inc" negatedConjunction

  -- The requirements are considered consistent if it was *not* possible to
  -- prove that their conjunction is always false.
  let consistent = not $ fst provedNegatedConjunction

  return $ Right $
    AnalysisResult numTrue numFalse trueReqs falseReqs consistent

-- * Auxiliary

-- ** Structured Copilot specifications

-- | A structured Copilot specification.
data CopilotSpec = CopilotSpec
  { copilotProperties :: [(String, String, String, String, String)]
                      -- ^ Requirement name, property name, handler name,
                      -- implementation and arguments.

  , copilotAuxDefs    :: [(String, String, String)]
                      -- ^ Name, type, implementation
  }

-- | Given a 'Spec', return a structured version the corresponding Copilot spec
-- that differentiates between the auxiliary definitions (inputs, outputs) and
-- the properties or requirements to check.
--
-- PRE: There are no name clashes between the variables and names used in the
-- specification and any definitions in Haskell's Prelude or in Copilot.
spec2Copilot :: [(String, String)]             -- Type substitution table
             -> ([(String, String)] -> a -> a) -- Expr substitution function
             -> (a -> String)                  -- Expr show function
             -> Spec a                         -- Specification
             -> CopilotSpec
spec2Copilot typeMaps exprTransform showExpr spec = CopilotSpec reqs auxDefs
  where
    -- Encoding of requirements as boolean streams
    reqs :: [(String, String, String, String, String)]
    reqs = map reqToDecl (requirements spec)
      where
        reqToDecl i =
            ( reqName
            , propName
            , handlerName
            , reqBody nameSubstitutions
            , handlerArg
            )
          where
            reqName = requirementName i

            propName = safeMap nameSubstitutions (requirementName i)

            handlerName = "handler" ++ sanitizeUCIdentifier (requirementName i)

            -- Definition implementation. We use an auxiliary function to
            -- transform the implementation into Copilot, applying a
            -- substitution.
            reqBody subs = showExpr (exprTransform subs (requirementExpr i))

            handlerArg  =
              case (requirementResultType i, requirementResultExpr i) of
                (Just _, Just ex) -> "[ arg (" ++ showExpr ex ++ " ) ]"
                _                 -> "[]"

    auxDefs :: [(String, String, String)]
    auxDefs = externs ++ internals

    externs :: [(String, String, String)]
    externs = map externVarToDecl (externalVariables spec)
      where
        externVarToDecl i = (propName, streamType, implementation)
          where
            propName = safeMap nameSubstitutions (externalVariableName i)

            streamType = "Stream " ++ "(" ++ valueType ++ ")"
            valueType  = safeMap typeMaps (externalVariableType i)

            implementation = "extern" ++ " " ++ show (externalVariableName i)
                          ++ " " ++ "Nothing"

    -- Internal stream definitions
    internals :: [(String, String, String)]
    internals = map internalVarToDecl (internalVariables spec)
      where
        internalVarToDecl i = (propName, streamType, implementation)
          where
            propName = safeMap nameSubstitutions (internalVariableName i)

            streamType = "Stream " ++ "(" ++ valueType ++ ")"
            valueType  = safeMap typeMaps (internalVariableType i)

            implementation = internalVariableExpr i

    nameSubstitutions = internalVariableMap
                     ++ externalVariableMap
                     ++ requirementNameMap

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

-- | Render a 'CopilotSpec' as a Haskell definition of a 'Copilot.Spec',
-- listing the elements in the spec with the necessary indentation.
--
-- The shown 'Copilot.Spec' has a list of top-level triggers, as well as
-- several auxiliary definitions.
showSpec :: CopilotSpec -> String
showSpec spec = template ++ "\n" ++ extra ++ "\n" ++ triggers
  where
    -- Initial template used for analysis purposes.
    template :: String
    template = unlines
      [ "do let"
      , ""
      , "       clock :: Stream Int64"
      , "       clock = [0] ++ (clock + 1)"
      , ""
      , "       ftp :: Stream Bool"
      , "       ftp = [True] ++ false"
      , ""
      , "       pre :: Stream Bool -> Stream Bool"
      , "       pre = ([False] ++)"
      , ""
      , "       tpre :: Stream Bool -> Stream Bool"
      , "       tpre = ([True] ++)"
      , ""
      , "       notPreviousNot :: Stream Bool -> Stream Bool"
      , "       notPreviousNot = not . PTLTL.previous . not"
      ]

    extra = unlines
          $ intercalate [""]
          $ map formatDef
          $ copilotAuxDefs spec

    triggers = unlines
             $ intercalate [""]
             $ map formatTrigger
             $ copilotProperties spec

    formatDef (n, t, i) =
      map ("       " ++) [ n ++ " :: " ++ t, n ++ " = " ++ i ]

    formatTrigger (_, _, h, g, a) =
      [ "   trigger " ++ show h ++ " (" ++ g ++ ") " ++ a ]

-- | Default imports for a 'Spec' that was converted into a 'Copilot.Spec'.
defaultSpecImports :: [(String, Maybe String)]
defaultSpecImports =
  [ ("Control.Monad.Writer",  Nothing)
  , ("Copilot.Language",      Nothing)
  , ("Copilot.Language.Spec", Nothing)
  , ("Data.Functor.Identity", Nothing)
  , ("Language.Copilot",      Nothing)
  , ("Copilot.Library.PTLTL", Just "PTLTL")
  , ("Prelude",               Just "P")
  ]

-- ** Auxiliary list functions

-- | Substitute a key based on a given substitution table from key to
-- alternative key.
--
-- They key is left unchanged if it cannot be found in the substitution table.
safeMap :: Eq k => [(k, k)] -> k -> k
safeMap ls k = fromMaybe k $ lookup k ls
