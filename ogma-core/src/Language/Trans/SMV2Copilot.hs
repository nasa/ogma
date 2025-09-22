-- Copyright 2020 United States Government as represented by the Administrator
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

-- | Transform an SMV TL specification into a Copilot specification.
--
-- Normally, this module would be implemented as a conversion between ASTs,
-- but we want to add comments to the generated code, which are not
-- representable in the abstract syntax tree.
module Language.Trans.SMV2Copilot where

import Language.SMV.AbsSMV (AdditiveOp (..), BoolConst (..), BoolSpec (..),
                            Ident (..), MultOp (..), NumExpr (..), Number (..),
                            Op1Name (..), OpOne (..), OpTwo (..), OrdOp (..))

-- | Return the Copilot representation of an SMV BoolSpec.
--
-- This function returns the temporal property only. The string does not
-- contain any top-level names, any imports, or auxiliary definitions that may
-- be required.
boolSpec2Copilot :: BoolSpec -> String
boolSpec2Copilot b = case b of
  BoolSpecConst bc -> const2Copilot bc

  BoolSpecNum nc -> numExpr2Copilot nc

  BoolSpecSignal i -> ident2Copilot i

  BoolSpecCmp spec1 op2 spec2 -> "(" ++ boolSpec2Copilot spec1
                              ++ " " ++ ordOp2Copilot op2
                              ++ " " ++ boolSpec2Copilot spec2
                              ++ ")"

  BoolSpecNeg spec -> "(" ++ "not"
                   ++ " " ++ boolSpec2Copilot spec
                   ++ ")"

  BoolSpecAnd spec1 spec2 -> "(" ++ boolSpec2Copilot spec1
                          ++ " " ++ "&&"
                          ++ " " ++ boolSpec2Copilot spec2
                          ++ ")"

  BoolSpecOr spec1 spec2 -> "(" ++ boolSpec2Copilot spec1
                         ++ " " ++ "||"
                         ++ " " ++ boolSpec2Copilot spec2
                         ++ ")"

  BoolSpecXor spec1 spec2 -> "(" ++ boolSpec2Copilot spec1
                          ++ " " ++ "`xor`"
                          ++ " " ++ boolSpec2Copilot spec2
                          ++ ")"

  BoolSpecImplies spec1 spec2 -> "(" ++ boolSpec2Copilot spec1
                              ++ " " ++ "==>"
                              ++ " " ++ boolSpec2Copilot spec2
                              ++ ")"

  BoolSpecEquivs spec1 spec2 -> "(" ++ boolSpec2Copilot spec1
                             ++ " " ++ "=="
                             ++ " " ++ boolSpec2Copilot spec2
                             ++ ")"

  BoolSpecOp1 op spec -> "(" ++ opOne2Copilot op
                      ++ " " ++ boolSpec2Copilot spec
                      ++ ")"

  BoolSpecOp2 spec1 op2 spec2 -> "(" ++ boolSpec2Copilot spec1
                              ++ " " ++ opTwo2Copilot op2
                              ++ " " ++ boolSpec2Copilot spec2
                              ++ ")"

-- | Return the Copilot representation of an SMV boolean constant.
const2Copilot :: BoolConst -> String
const2Copilot BoolConstTrue  = "true"
const2Copilot BoolConstFalse = "false"
const2Copilot BoolConstFTP   = "ftp"
const2Copilot BoolConstLAST  = "last"

-- | Return the Copilot representation of a numeric expression.
numExpr2Copilot :: NumExpr -> String
numExpr2Copilot (NumId i)        = ident2Copilot i
numExpr2Copilot (NumConstI i)    = show i
numExpr2Copilot (NumConstD i)    = show i
numExpr2Copilot (NumAdd x op y)  = "("
                                   ++ numExpr2Copilot x
                                   ++ additiveOp2Copilot op
                                   ++ numExpr2Copilot y
                                   ++ ")"
numExpr2Copilot (NumMult x op y) = "("
                                   ++ numExpr2Copilot x
                                   ++ multOp2Copilot op
                                   ++ numExpr2Copilot y
                                   ++ ")"

-- | Return the Copilot representation of an SMV additive operator.
additiveOp2Copilot :: AdditiveOp -> String
additiveOp2Copilot OpPlus  = "+"
additiveOp2Copilot OpMinus = "-"

-- | Return the Copilot representation of an SMV multiplicative operator.
multOp2Copilot :: MultOp -> String
multOp2Copilot OpTimes = "*"
multOp2Copilot OpDiv   = "/"

-- | Return the Copilot representation of an SMV comparison operator.
ordOp2Copilot :: OrdOp -> String
ordOp2Copilot OrdOpLT = "<"
ordOp2Copilot OrdOpLE = "<="
ordOp2Copilot OrdOpEQ = "=="
ordOp2Copilot OrdOpNE = "/="
ordOp2Copilot OrdOpGT = ">"
ordOp2Copilot OrdOpGE = ">="

-- | Return the Copilot representation of a unary logical SMV operator.
opOne2Copilot :: OpOne -> String
opOne2Copilot (Op1Alone x)    = opOneAlone2Copilot x
opOne2Copilot (Op1MTL x op v) = opOneMTL2Copilot x op v
opOne2Copilot (Op1MTLRange op mn mx) = opOneMTLRange2Copilot op mn mx

-- | Return the Copilot representation of a unary logical non-MTL SMV
-- operator.
opOneAlone2Copilot :: Op1Name -> String
opOneAlone2Copilot Op1Pre  = "pre"
opOneAlone2Copilot Op1X    = "next"
opOneAlone2Copilot Op1G    = "always"
opOneAlone2Copilot Op1F    = "eventually"
opOneAlone2Copilot Op1Y    = "PTLTL.previous"
opOneAlone2Copilot Op1Z    = "notPreviousNot"
opOneAlone2Copilot Op1Hist = "PTLTL.alwaysBeen"
opOneAlone2Copilot Op1O    = "PTLTL.eventuallyPrev"

-- | Return the Copilot representation of a unary logical MTL SMV operator.
opOneMTL2Copilot :: Op1Name -> OrdOp -> Number -> String
opOneMTL2Copilot operator _comparison number =
  opOneMTL2Copilot' operator ++ " " ++ show (0 :: Int)
                             ++ " " ++ number2Copilot number
                             ++ " " ++ "clock" ++ " "
                             ++ show (1 :: Int)

-- | Return the Copilot representation of a unary logical MTL SMV operator
-- that uses an explicit range.
opOneMTLRange2Copilot :: Op1Name -> Number -> Number -> String
opOneMTLRange2Copilot operator mn mx =
  opOneMTL2Copilot' operator ++ " " ++ number2Copilot mn
                             ++ " " ++ number2Copilot mx
                             ++ " " ++ "clock" ++ " "
                             ++ show (1 :: Int)

-- | Return the Copilot representation of a unary logical possibly MTL SMV
-- operator.
opOneMTL2Copilot' :: Op1Name -> String
opOneMTL2Copilot' Op1Pre  = "pre"
opOneMTL2Copilot' Op1X    = "next"
opOneMTL2Copilot' Op1G    = "always"
opOneMTL2Copilot' Op1F    = "eventually"
opOneMTL2Copilot' Op1Y    = "MTL.previous"
opOneMTL2Copilot' Op1Z    = "notPreviousNot"
opOneMTL2Copilot' Op1Hist = "MTL.alwaysBeen"
opOneMTL2Copilot' Op1O    = "MTL.eventuallyPrev"

-- | Return the Copilot representation of an SMV number.
number2Copilot :: Number -> String
number2Copilot (NumberInt n) = show n

-- | Return the Copilot representation of a binary logical non-MTL SMV
-- operator.
opTwo2Copilot :: OpTwo -> String
opTwo2Copilot Op2S = "`since`"
opTwo2Copilot Op2T = "`triggers`"
opTwo2Copilot Op2V = "`releases`"
opTwo2Copilot Op2U = "`until`"

-- | Return the Copilot representation of an SMV identifier.
ident2Copilot :: Ident -> String
ident2Copilot (Ident i) = i

-- | Return all identifiers used in a BoolSpec that are not reserved keywords.
boolSpecNames :: BoolSpec -> [String]
boolSpecNames b = case b of
  BoolSpecConst _bc            -> []
  BoolSpecSignal (Ident i)     -> [i]
  BoolSpecNum e                -> numExprNames e
  BoolSpecCmp spec1 _op2 spec2 -> boolSpecNames spec1 ++ boolSpecNames spec2
  BoolSpecNeg spec             -> boolSpecNames spec
  BoolSpecAnd spec1 spec2      -> boolSpecNames spec1 ++ boolSpecNames spec2
  BoolSpecOr  spec1 spec2      -> boolSpecNames spec1 ++ boolSpecNames spec2
  BoolSpecXor spec1 spec2      -> boolSpecNames spec1 ++ boolSpecNames spec2
  BoolSpecImplies spec1 spec2  -> boolSpecNames spec1 ++ boolSpecNames spec2
  BoolSpecEquivs spec1 spec2   -> boolSpecNames spec1 ++ boolSpecNames spec2
  BoolSpecOp1 _op spec         -> boolSpecNames spec
  BoolSpecOp2 spec1 _op2 spec2 -> boolSpecNames spec1 ++ boolSpecNames spec2

-- | Return all identifiers used in a numeric expression.
numExprNames :: NumExpr -> [String]
numExprNames numExpr = case numExpr of
  NumId (Ident i)         -> [i]
  NumConstI _c            -> []
  NumConstD _c            -> []
  NumAdd expr1 _op expr2  -> numExprNames expr1 ++ numExprNames expr2
  NumMult expr1 _op expr2 -> numExprNames expr1 ++ numExprNames expr2
