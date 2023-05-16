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
-- | Transform a CoCoSpec TL specification into a R2U2 specification.
--
-- Normally, this module would be implemented as a conversion between ASTs,
-- but we want to add comments to the generated code, which are not
-- representable in the abstract syntax tree.
module Language.Trans.CoCoSpec2R2U2 (boolSpec2R2U2, boolSpecNames) where

-- Internal imports
import Language.CoCoSpec.AbsCoCoSpec ( BoolConst (..), BoolNumOp (..),
                                       BoolSpec (..), Ident (..), NumExpr (..),
                                       NumOp2In (..), Op1Pre (..), Op2In (..),
                                       Op2Pre (..) )

-- | Return the R2U2 representation of a CoCoSpec 'BoolSpec'.
--
-- This function returns the temporal property only. The string does not
-- contain any top-level names, any imports, or auxiliary definitions that
-- may be required.
boolSpec2R2U2 :: BoolSpec -> String
boolSpec2R2U2 b = case b of
  BoolSpecPar bs                 -> "( " ++ boolSpec2R2U2 bs ++ " )"
  BoolSpecConstI bc              -> show bc
  BoolSpecConstD bc              -> show bc
  BoolSpecConstB bc              -> const2R2U2 bc
  BoolSpecSignal i               -> ident2R2U2 i
  BoolSpecOp1Pre op spec         -> opOnePre2R2U2 op ++ " (" ++ boolSpec2R2U2 spec ++ ")"

  BoolSpecOp2In spec1 Op2InPre (BoolSpecOp1Pre Op1Pre spec2)
    -> "[" ++ lit2R2U2 spec1 ++ "] ++ " ++ boolSpec2R2U2 spec2

  BoolSpecOp2In spec1 Op2InPre spec2
    -> "mux ftp (constant " ++ lit2R2U2 spec1 ++ ") (" ++ boolSpec2R2U2 spec2 ++ ")"

  BoolSpecOp2In spec1 op2 spec2  -> "(" ++ boolSpec2R2U2 spec1
                                 ++ " " ++ opTwoIn2R2U2 op2
                                 ++ " " ++ boolSpec2R2U2 spec2
                                 ++ ")"

  BoolSpecOp2Pre op2 spec1 spec2 -> opTwoPre2R2U2 op2 ++ " " ++ boolSpec2R2U2 spec1
                                                    ++ " " ++ boolSpec2R2U2 spec2

  BoolSpecOp2HT  num1 num2 spec  -> "MTL.alwaysBeen"
                                       ++ " " ++ numExpr2R2U2 num2
                                       ++ " " ++ numExpr2R2U2 num1
                                       ++ " clock 1" -- clock and min time distance
                                       ++ " " ++ boolSpec2R2U2 spec

  BoolSpecOp2OT  num1 num2 spec  -> "MTL.eventuallyPrev"
                                       ++ " " ++ numExpr2R2U2 num2
                                       ++ " " ++ numExpr2R2U2 num1
                                       ++ " clock 1" -- clock and min time distance
                                       ++ " " ++ boolSpec2R2U2 spec

  BoolSpecOp2ST  num1 num2 spec1 spec2 -> "MTL.since"
                                             ++ " " ++ numExpr2R2U2 num1
                                             ++ " " ++ numExpr2R2U2 num2
                                             ++ " clock 1" -- clock and min time distance
                                             ++ " " ++ boolSpec2R2U2 spec1
                                             ++ " " ++ boolSpec2R2U2 spec2

-- | Return the R2U2 representation of a CoCoSpec numeric
-- expression.
--
-- This function returns the expression only. The string does not contain any
-- top-level names, any imports, or auxiliary definitions that may be required.
numExpr2R2U2 :: NumExpr -> String
numExpr2R2U2 expr = case expr of
  NumExprNum i                  -> show i
  NumExprPar iExpr              -> "(" ++ numExpr2R2U2 iExpr  ++ ")"
  NumExprOp2In iExpr1 op iExpr2 -> "(" ++ numExpr2R2U2 iExpr1 ++ " "
                                       ++ numOpTwoIn2R2U2 op    ++ " "
                                       ++ numExpr2R2U2 iExpr2 ++ ")"
  NumExprId i                   -> ident2R2U2 i

-- | Return the R2U2 representation of a numeric CoCoSpec arithmetic
-- operator.
numOpTwoIn2R2U2 :: NumOp2In -> String
numOpTwoIn2R2U2 NumOp2Plus  = "+"
numOpTwoIn2R2U2 NumOp2Minus = "-"
numOpTwoIn2R2U2 NumOp2Mult  = "*"

-- | Return the R2U2 representation of a numeric CoCoSpec comparison
-- operator.
opTwoNum2R2U2 :: BoolNumOp -> String
opTwoNum2R2U2 BoolNumOp2Eq = "=="
opTwoNum2R2U2 BoolNumOp2Ne = "/="
opTwoNum2R2U2 BoolNumOp2Le = "<="
opTwoNum2R2U2 BoolNumOp2Lt = "<"
opTwoNum2R2U2 BoolNumOp2Gt = ">="
opTwoNum2R2U2 BoolNumOp2Ge = ">"

-- | Return the R2U2 representation of a CoCoSpec boolean
-- constant.
const2R2U2 :: BoolConst -> String
const2R2U2 BoolConstTrue  = "true"
const2R2U2 BoolConstFalse = "false"
const2R2U2 BoolConstFTP   = "ftp"

-- | Return the R2U2 representation of a CoCoSpec logical
-- operator.
opOnePre2R2U2 :: Op1Pre -> String
opOnePre2R2U2 Op1Pre    = "pre"
opOnePre2R2U2 Op1Once   = "PTLTL.eventuallyPrev"
opOnePre2R2U2 Op1Hist   = "PTLTL.alwaysBeen"
opOnePre2R2U2 Op1Y      = "PTLTL.previous"
opOnePre2R2U2 Op1Not    = "not"
opOnePre2R2U2 Op1Bang   = "not"

-- | Return the R2U2 representation of a CoCoSpec logical
-- operator.
opTwoIn2R2U2 :: Op2In -> String
opTwoIn2R2U2 Op2Amp   = "&&"
opTwoIn2R2U2 Op2And   = "&&"
opTwoIn2R2U2 Op2Or    = "||"
opTwoIn2R2U2 Op2Impl  = "==>"
opTwoIn2R2U2 Op2InPre = "pre"
opTwoIn2R2U2 (Op2NumOp n) = numOpTwoIn2R2U2 n
opTwoIn2R2U2 (Op2NumCmp n) = opTwoNum2R2U2 n

-- | Return the R2U2 representation of a CoCoSpec logical
-- operator.
opTwoPre2R2U2 :: Op2Pre -> String
opTwoPre2R2U2 Op2SI = "since"
opTwoPre2R2U2 Op2OT = "ot"

-- | Return the R2U2 representation of a CoCoSpec identifier.
ident2R2U2 :: Ident -> String
ident2R2U2 (Ident "FTP") = "ftp"
ident2R2U2 (Ident s)     = s

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

-- | Return the R2U2 representation of a CoCoSpec literal.
lit2R2U2 :: BoolSpec -> String
lit2R2U2 b = case b of
    BoolSpecConstI bc -> show bc
    BoolSpecConstD bc -> show bc
    BoolSpecConstB bc -> litConst2R2U2 bc
    BoolSpecSignal i  -> ident2R2U2 i
    _                 -> ":error converting literal:"
  where
    -- | Return the R2U2 representation of a CoCoSpec boolean
    -- constant.
    litConst2R2U2 :: BoolConst -> String
    litConst2R2U2 BoolConstTrue  = "True"
    litConst2R2U2 BoolConstFalse = "False"
    litConst2R2U2 _              = ":error converting literal boolean:"
