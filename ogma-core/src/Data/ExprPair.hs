{-# LANGUAGE ExistentialQuantification #-}
-- Copyright 2022 United States Government as represented by the Administrator
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
--
-- | Abstraction for expressions used in specifications.
module Data.ExprPair
    ( ExprPair(..)
    , ExprPairT(..)
    , exprPair
    , exprPairShow
    )
  where

-- External imports
import qualified Language.Lustre.AbsLustre     as Lustre
import qualified Language.Lustre.ParLustre     as Lustre (myLexer, pBoolSpec)
import qualified Language.SMV.AbsSMV           as SMV
import qualified Language.SMV.ParSMV           as SMV (myLexer, pBoolSpec)
import           Language.SMV.Substitution     (substituteBoolExpr)
import qualified Language.Trans.Lustre2Copilot as Lustre (boolSpec2Copilot,
                                                          boolSpecNames)
import           Language.Trans.SMV2Copilot    as SMV (boolSpec2Copilot,
                                                       boolSpecNames)

-- | Existential wrapper over abstraction to handle expressions used in
-- specifications.
--
-- The abstraction provides mechanisms to parse expressions, replace variables
-- in them, render them in a known format or notation, and find identifiers in
-- them, and it provides a default value to use when an expression is needed.
data ExprPair = forall a . ExprPair
  { exprTPair :: ExprPairT a
  }

-- | Abstraction to handle expressions used in specifications.
--
-- The abstraction provides mechanisms to parse expressions, replace variables
-- in them, render them in a known format or notation, and find identifiers in
-- them, and it provides a default value to use when an expression is needed.
data ExprPairT a = ExprPairT
  { exprTParse   :: String -> Either String a
  , exprTReplace :: [(String, String)] -> a -> a
  , exprTPrint   :: a -> String
  , exprTIdents  :: a -> [String]
  , exprTUnknown :: a
  }

-- | Return 'ExprPair' for a given language (e.g., Lustre, SMV).
--
-- The language name must be lowercase.
--
-- We default to SMV if no format is given.
--
-- The format literal returns an ExprPair that uses Strings as the base type
-- and keeps them unchanged.
exprPair :: String -> ExprPair
exprPair "lustre" = ExprPair $
  ExprPairT
    (Lustre.pBoolSpec . Lustre.myLexer)
    (const id)
    Lustre.boolSpec2Copilot
    Lustre.boolSpecNames
    (Lustre.BoolSpecSignal (Lustre.Ident "undefined"))
exprPair "literal" = ExprPair $
  ExprPairT
    Right
    (\_ -> id)
    id
    (const [])
    "undefined"
exprPair "cocospec" = exprPair "lustre"
exprPair _ = ExprPair $
  ExprPairT
    (SMV.pBoolSpec . SMV.myLexer)
    substituteBoolExpr
    SMV.boolSpec2Copilot
    SMV.boolSpecNames
    (SMV.BoolSpecSignal (SMV.Ident "undefined"))

-- | Parse and print a value using an auxiliary Expression Pair.
--
-- Fails if the value has no valid parse.
exprPairShow :: ExprPair -> String -> String
exprPairShow (ExprPair exprP) =
    printProp . fromRight' . parseProp
  where
    ExprPairT parseProp _replace printProp _ids _unknown = exprP

    -- | Unsafe fromRight. Fails if the value is a 'Left'.
    fromRight' :: Either a b -> b
    fromRight' (Right v) = v
    fromRight' _         = error "fromRight' applied to Left value."
