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
-- | Transform a Lustre specification, extended with temporal logic operators,
-- into a Copilot specification.
--
-- Normally, this module would be implemented as a conversion between ASTs,
-- but we want to add comments to the generated code, which are not
-- representable in the abstract syntax tree.
module Language.Trans.Lustre2Copilot (boolSpec2Copilot, boolSpecNames) where

-- Internal imports
import Language.Lustre.AbsLustre (BoolConst (..), BoolNumOp (..), BoolSpec (..),
                                  Ident (..), NumExpr (..), NumOp2In (..),
                                  Op1Pre (..), Op2In (..), Op2Pre (..))

-- | Return the Copilot representation of a Lustre 'BoolSpec'.
--
-- This function returns the temporal property only. The string does not
-- contain any top-level names, any imports, or auxiliary definitions that
-- may be required.
boolSpec2Copilot :: BoolSpec -> String
boolSpec2Copilot b = case b of
  BoolSpecPar bs                 -> "( " ++ boolSpec2Copilot bs ++ " )"
  BoolSpecConstI bc              -> show bc
  BoolSpecConstD bc              -> show bc
  BoolSpecConstB bc              -> const2Copilot bc
  BoolSpecSignal i               -> ident2Copilot i
  BoolSpecOp1Pre op spec         -> opOnePre2Copilot op ++ " (" ++ boolSpec2Copilot spec ++ ")"

  BoolSpecOp2In spec1 Op2InPre (BoolSpecOp1Pre Op1Pre spec2)
    -> "[" ++ lit2Copilot spec1 ++ "] ++ " ++ boolSpec2Copilot spec2

  BoolSpecOp2In spec1 Op2InPre spec2
    -> "mux ftp (constant " ++ lit2Copilot spec1 ++ ") (" ++ boolSpec2Copilot spec2 ++ ")"

  BoolSpecOp2In spec1 op2 spec2  -> "(" ++ boolSpec2Copilot spec1
                                 ++ " " ++ opTwoIn2Copilot op2
                                 ++ " " ++ boolSpec2Copilot spec2
                                 ++ ")"

  BoolSpecOp2Pre op2 spec1 spec2 -> opTwoPre2Copilot op2 ++ " " ++ boolSpec2Copilot spec1
                                                    ++ " " ++ boolSpec2Copilot spec2

  BoolSpecOp2HT  num1 num2 spec  -> "MTL.alwaysBeen"
                                       ++ " " ++ numExpr2Copilot num2
                                       ++ " " ++ numExpr2Copilot num1
                                       ++ " clock 1" -- clock and min time distance
                                       ++ " " ++ boolSpec2Copilot spec

  BoolSpecOp2OT  num1 num2 spec  -> "MTL.eventuallyPrev"
                                       ++ " " ++ numExpr2Copilot num2
                                       ++ " " ++ numExpr2Copilot num1
                                       ++ " clock 1" -- clock and min time distance
                                       ++ " " ++ boolSpec2Copilot spec

  BoolSpecOp2ST  num1 num2 spec1 spec2 -> "MTL.since"
                                             ++ " " ++ numExpr2Copilot num1
                                             ++ " " ++ numExpr2Copilot num2
                                             ++ " clock 1" -- clock and min time distance
                                             ++ " " ++ boolSpec2Copilot spec1
                                             ++ " " ++ boolSpec2Copilot spec2

-- | Return the Copilot representation of a Lustre numeric
-- expression.
--
-- This function returns the expression only. The string does not contain any
-- top-level names, any imports, or auxiliary definitions that may be required.
numExpr2Copilot :: NumExpr -> String
numExpr2Copilot expr = case expr of
  NumExprNum i                  -> show i
  NumExprPar iExpr              -> "(" ++ numExpr2Copilot iExpr  ++ ")"
  NumExprOp2In iExpr1 op iExpr2 -> "(" ++ numExpr2Copilot iExpr1 ++ " "
                                       ++ numOpTwoIn2Copilot op    ++ " "
                                       ++ numExpr2Copilot iExpr2 ++ ")"
  NumExprId i                   -> ident2Copilot i

-- | Return the Copilot representation of a numeric Lustre arithmetic
-- operator.
numOpTwoIn2Copilot :: NumOp2In -> String
numOpTwoIn2Copilot NumOp2Plus  = "+"
numOpTwoIn2Copilot NumOp2Minus = "-"
numOpTwoIn2Copilot NumOp2Mult  = "*"

-- | Return the Copilot representation of a numeric Lustre comparison
-- operator.
opTwoNum2Copilot :: BoolNumOp -> String
opTwoNum2Copilot BoolNumOp2Eq = "=="
opTwoNum2Copilot BoolNumOp2Ne = "/="
opTwoNum2Copilot BoolNumOp2Le = "<="
opTwoNum2Copilot BoolNumOp2Lt = "<"
opTwoNum2Copilot BoolNumOp2Gt = ">="
opTwoNum2Copilot BoolNumOp2Ge = ">"

-- | Return the Copilot representation of a Lustre boolean
-- constant.
const2Copilot :: BoolConst -> String
const2Copilot BoolConstTrue  = "true"
const2Copilot BoolConstFalse = "false"
const2Copilot BoolConstFTP   = "ftp"

-- | Return the Copilot representation of a Lustre logical
-- operator.
opOnePre2Copilot :: Op1Pre -> String
opOnePre2Copilot Op1Pre    = "pre"
opOnePre2Copilot Op1YtoPre = "pre"
opOnePre2Copilot Op1ZtoPre = "tpre"
opOnePre2Copilot Op1Once   = "PTLTL.eventuallyPrev"
opOnePre2Copilot Op1Hist   = "PTLTL.alwaysBeen"
opOnePre2Copilot Op1Y      = "PTLTL.previous"
opOnePre2Copilot Op1Not    = "not"
opOnePre2Copilot Op1Bang   = "not"

-- | Return the Copilot representation of a Lustre logical
-- operator.
opTwoIn2Copilot :: Op2In -> String
opTwoIn2Copilot Op2Amp   = "&&"
opTwoIn2Copilot Op2And   = "&&"
opTwoIn2Copilot Op2Or    = "||"
opTwoIn2Copilot Op2Impl  = "==>"
opTwoIn2Copilot Op2InPre = "pre"
opTwoIn2Copilot (Op2NumOp n) = numOpTwoIn2Copilot n
opTwoIn2Copilot (Op2NumCmp n) = opTwoNum2Copilot n

-- | Return the Copilot representation of a Lustre logical
-- operator.
opTwoPre2Copilot :: Op2Pre -> String
opTwoPre2Copilot Op2SI = "since"
opTwoPre2Copilot Op2OT = "ot"

-- | Return the Copilot representation of a Lustre identifier.
ident2Copilot :: Ident -> String
ident2Copilot (Ident "FTP") = "ftp"
ident2Copilot (Ident s)     = s

-- | Return all identifiers used in a BoolSpec that are not reserved keywords.
boolSpecNames :: BoolSpec -> [String]
boolSpecNames (BoolSpecPar bs)                  = boolSpecNames bs
boolSpecNames (BoolSpecConstI _bc)              = []
boolSpecNames (BoolSpecConstD _bc)              = []
boolSpecNames (BoolSpecConstB _bc)              = []
boolSpecNames (BoolSpecSignal (Ident i))        = [i]
boolSpecNames (BoolSpecOp1Pre _op spec)         = boolSpecNames spec
boolSpecNames (BoolSpecOp2In  spec1 _op2 spec2) = boolSpecNames spec1 ++ boolSpecNames spec2
boolSpecNames (BoolSpecOp2Pre _op2 spec1 spec2) = boolSpecNames spec1 ++ boolSpecNames spec2
boolSpecNames (BoolSpecOp2HT  num1 num2 spec)   =
  numExprNames num1 ++ numExprNames num2 ++ boolSpecNames spec
boolSpecNames (BoolSpecOp2OT  num1 num2 spec)   =
  numExprNames num1 ++ numExprNames num2 ++ boolSpecNames spec
boolSpecNames (BoolSpecOp2ST  num1 num2 spec1 spec2)   =
  numExprNames num1 ++ numExprNames num2 ++ boolSpecNames spec1 ++ boolSpecNames spec2

-- | Return all identifiers used in a NumExpr that are not reserved keywords.
numExprNames :: NumExpr -> [String]
numExprNames (NumExprNum _i)                = []
numExprNames (NumExprPar expr)              = numExprNames expr
numExprNames (NumExprOp2In expr1 _op expr2) =
  numExprNames expr1 ++ numExprNames expr2
numExprNames (NumExprId (Ident i))          = [i]

-- | Return the Copilot representation of a Lustre literal.
lit2Copilot :: BoolSpec -> String
lit2Copilot b = case b of
    BoolSpecConstI bc -> show bc
    BoolSpecConstD bc -> show bc
    BoolSpecConstB bc -> litConst2Copilot bc
    BoolSpecSignal i  -> ident2Copilot i
    _                 -> ":error converting literal:"
  where
    -- | Return the Copilot representation of a Lustre boolean
    -- constant.
    litConst2Copilot :: BoolConst -> String
    litConst2Copilot BoolConstTrue  = "True"
    litConst2Copilot BoolConstFalse = "False"
    litConst2Copilot _              = ":error converting literal boolean:"
