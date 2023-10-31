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

-- | Transform a FRET TL specification into a R2U2 specification.
--
-- Normally, this module would be implemented as a conversion between ASTs,
-- but we want to add comments to the generated code, which are not
-- representable in the abstract syntax tree.
module Language.Trans.SMV2R2U2 where

import Language.SMV.AbsSMV (AdditiveOp (..), BoolConst (..), BoolSpec (..),
                            Ident (..), MultOp (..), NumExpr (..), Number (..),
                            Op1Name (..), OpOne (..), OpTwo (..), OrdOp (..))

-- | Return the R2U2 representation of a FRET BoolSpec.
--
-- This function returns the temporal property only. The string does not
-- contain any top-level names, any imports, or auxiliary definitions that may
-- be required.
boolSpec2R2U2 :: BoolSpec -> String
boolSpec2R2U2 b = case b of
  BoolSpecConst bc -> const2R2U2 bc

  BoolSpecNum nc -> numExpr2R2U2 nc

  BoolSpecSignal i -> ident2R2U2 i

  BoolSpecCmp spec1 op2 spec2 -> "(" ++ boolSpec2R2U2 spec1
                              ++ " " ++ ordOp2R2U2 op2
                              ++ " " ++ boolSpec2R2U2 spec2
                              ++ ")"

  BoolSpecNeg spec -> "(" ++ "!"
                   ++ " " ++ boolSpec2R2U2 spec
                   ++ ")"

  BoolSpecAnd spec1 spec2 -> "(" ++ boolSpec2R2U2 spec1
                          ++ " " ++ "&&"
                          ++ " " ++ boolSpec2R2U2 spec2
                          ++ ")"

  BoolSpecOr spec1 spec2 -> "(" ++ boolSpec2R2U2 spec1
                         ++ " " ++ "||"
                         ++ " " ++ boolSpec2R2U2 spec2
                         ++ ")"

  BoolSpecXor spec1 spec2 -> "(" ++ boolSpec2R2U2 spec1
                          ++ " " ++ "||"
                          ++ " ! (" ++ boolSpec2R2U2 spec2
                          ++ ")) && "
                          ++ "(!( " ++ boolSpec2R2U2 spec1
                          ++ ") " ++ "||"
                          ++ " " ++ boolSpec2R2U2 spec2
                          ++ ")"

  BoolSpecImplies spec1 spec2 -> "(" ++ boolSpec2R2U2 spec1
                              ++ " " ++ "->"
                              ++ " " ++ boolSpec2R2U2 spec2
                              ++ ")"

  BoolSpecEquivs spec1 spec2 -> "(" ++ boolSpec2R2U2 spec1
                             ++ " " ++ "<->"
                             ++ " " ++ boolSpec2R2U2 spec2
                             ++ ")"

  BoolSpecOp1 op spec -> "(" ++ opOne2R2U2 op
                      ++ " " ++ boolSpec2R2U2 spec
                      ++ ")"

  BoolSpecOp2 spec1 op2 spec2 -> "(" ++ boolSpec2R2U2 spec1
                              ++ " " ++ opTwo2R2U2 op2
                              ++ " " ++ boolSpec2R2U2 spec2
                              ++ ")"

-- | Return the R2U2 representation of a FRET boolean constant.
const2R2U2 :: BoolConst -> String
const2R2U2 BoolConstTrue  = "true"
const2R2U2 BoolConstFalse = "false"
const2R2U2 BoolConstFTP   = "ftp"
const2R2U2 BoolConstLAST  = "last"

-- | Return the R2U2 representation of a numeric expression.
numExpr2R2U2 :: NumExpr -> String
numExpr2R2U2 (NumId i)        = ident2R2U2 i
numExpr2R2U2 (NumConstI i)    = show i
numExpr2R2U2 (NumConstD i)    = show i
numExpr2R2U2 (NumAdd x op y)  = "("
                                   ++ numExpr2R2U2 x
                                   ++ additiveOp2R2U2 op
                                   ++ numExpr2R2U2 y
                                   ++ ")"
numExpr2R2U2 (NumMult x op y) = "("
                                   ++ numExpr2R2U2 x
                                   ++ multOp2R2U2 op
                                   ++ numExpr2R2U2 y
                                   ++ ")"

-- | Return the R2U2 representation of a FRET additive operator.
additiveOp2R2U2 :: AdditiveOp -> String
additiveOp2R2U2 OpPlus  = "+"
additiveOp2R2U2 OpMinus = "-"

-- | Return the R2U2 representation of a FRET multiplicative operator.
multOp2R2U2 :: MultOp -> String
multOp2R2U2 OpTimes = "*"
multOp2R2U2 OpDiv   = "/"

-- | Return the R2U2 representation of a FRET comparison operator.
ordOp2R2U2 :: OrdOp -> String
ordOp2R2U2 OrdOpLT = "<"
ordOp2R2U2 OrdOpLE = "<="
ordOp2R2U2 OrdOpEQ = "=="
ordOp2R2U2 OrdOpNE = "!="
ordOp2R2U2 OrdOpGT = ">"
ordOp2R2U2 OrdOpGE = ">="

-- | Return the R2U2 representation of a unary logical FRET operator.
opOne2R2U2 :: OpOne -> String
opOne2R2U2 (Op1Alone x)          = opOneAlone2R2U2 x
opOne2R2U2 (Op1MTL x op v)       = opOneMTL2R2U2 x op v
opOne2R2U2 (Op1MTLRange x v1 v2) = opOneMTL2R2U2'' x v1 v2

-- | Return the R2U2 representation of a unary logical non-MTL FRET
-- operator.
opOneAlone2R2U2 :: Op1Name -> String
opOneAlone2R2U2 Op1Pre  = "H[1,1]"    -- double check
opOneAlone2R2U2 Op1X    = "G[1,1]"
opOneAlone2R2U2 Op1G    = "G[0," ++ show missionTime ++ "]"
opOneAlone2R2U2 Op1F    = "F[0," ++ show missionTime ++ "]"
opOneAlone2R2U2 Op1Y    = "H[1,1]"    -- double check
opOneAlone2R2U2 Op1Z    = "H[1,1]"    -- double check
opOneAlone2R2U2 Op1Hist = "H[0," ++ show missionTime ++ "]"
opOneAlone2R2U2 Op1O    = "O[0," ++ show missionTime ++ "]"

-- | Return the R2U2 representation of a unary logical MTL FRET operator.
opOneMTL2R2U2 :: Op1Name -> OrdOp -> Number -> String
opOneMTL2R2U2 operator _comparison number =
  opOneMTL2R2U2' operator ++ "[" ++ show (0 :: Int)
                             ++ "," ++ number2R2U2 number
                             ++ "]"

-- | Return the R2U2 representation of a unary logical MTL FRET operator.
opOneMTL2R2U2'' :: Op1Name -> Number -> Number -> String
opOneMTL2R2U2'' operator n1 n2 =
  opOneMTL2R2U2' operator ++ "[" ++ number2R2U2 n1
                          ++ "," ++ number2R2U2 n2
                          ++ "]"

-- | Return the R2U2 representation of a unary logical possibly MTL FRET
-- operator.
opOneMTL2R2U2' :: Op1Name -> String
opOneMTL2R2U2' Op1Pre  = "P"
opOneMTL2R2U2' Op1X    = "N"
opOneMTL2R2U2' Op1G    = "G"
opOneMTL2R2U2' Op1F    = "F"
opOneMTL2R2U2' Op1Y    = "Y"
opOneMTL2R2U2' Op1Z    = "Z"
opOneMTL2R2U2' Op1Hist = "H"
opOneMTL2R2U2' Op1O    = "O"

-- | Return the R2U2 representation of a FRET number.
number2R2U2 :: Number -> String
number2R2U2 (NumberInt n) = show n

-- | Return the R2U2 representation of a binary logical non-MTL FRET
-- operator.
opTwo2R2U2 :: OpTwo -> String
opTwo2R2U2 Op2S = "S[0,]" ++ show missionTime ++ "]"
opTwo2R2U2 Op2T = "T[0,]" ++ show missionTime ++ "]"
opTwo2R2U2 Op2V = "R[0,]" ++ show missionTime ++ "]"
opTwo2R2U2 Op2U = "U[0,]" ++ show missionTime ++ "]"

-- | Return the R2U2 representation of a FRET identifier.
ident2R2U2 :: Ident -> String
ident2R2U2 (Ident i) = i

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

missionTime = 100
