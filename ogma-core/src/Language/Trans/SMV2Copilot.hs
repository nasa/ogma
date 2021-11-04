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

-- | Transform a FRET TL specification into a Copilot specification.
--
-- Normally, this module would be implemented as a conversion between ASTs,
-- but we want to add comments to the generated code, which are not
-- representable in the abstract syntax tree.
module Language.Trans.SMV2Copilot where

import Language.SMV.AbsSMV ( BoolConst (..), BoolSpec (..), Ident (..),
                             NumConst (..), Number (..), Op1Name (..),
                             OpOne (..), OpTwo (..), OrdOp (..) )

-- | Return the Copilot representation of a FRET BoolSpec.
--
-- This function returns the temporal property only. The string does not
-- contain any top-level names, any imports, or auxiliary definitions that may
-- be required.
boolSpec2Copilot :: BoolSpec -> String
boolSpec2Copilot b = case b of
  BoolSpecPar bs -> "( " ++ boolSpec2Copilot bs ++ " )"

  BoolSpecBPar bs -> boolSpec2Copilot bs

  BoolSpecIPar bs -> boolSpec2Copilot bs

  BoolSpecConst bc -> const2Copilot bc

  BoolSpecNum   bc -> num2Copilot bc

  BoolSpecSignal i -> ident2Copilot i

  BoolSpecOp1 op spec ->
    opOne2Copilot op ++ " (" ++ boolSpec2Copilot spec ++ ")"

  BoolSpecOp2 spec1 op2 spec2 -> "(" ++ boolSpec2Copilot spec1
                              ++ " " ++ opTwo2Copilot op2
                              ++ " " ++ boolSpec2Copilot spec2
                              ++ ")"

-- | Return the Copilot representation of a FRET boolean constant.
const2Copilot :: BoolConst -> String
const2Copilot BoolConstTrue  = "true"
const2Copilot BoolConstFalse = "false"
const2Copilot BoolConstFTP   = "ftp"
const2Copilot BoolConstLAST  = "last"

-- | Return the Copilot representation of a numeric constant.
num2Copilot :: NumConst -> String
num2Copilot (NumConst i) = show i

-- | Return the Copilot representation of a unary logical FRET operator.
opOne2Copilot :: OpOne -> String
opOne2Copilot (Op1Alone x)    = opOneAlone2Copilot x
opOne2Copilot (Op1MTL x op v) = opOneMTL2Copilot x op v

-- | Return the Copilot representation of a unary logical non-MTL FRET
-- operator.
opOneAlone2Copilot :: Op1Name -> String
opOneAlone2Copilot Op1Not  = "not"
opOneAlone2Copilot Op1Pre  = "pre"
opOneAlone2Copilot Op1X    = "next"
opOneAlone2Copilot Op1G    = "always"
opOneAlone2Copilot Op1F    = "eventually"
opOneAlone2Copilot Op1Y    = "PTLTL.previous"
opOneAlone2Copilot Op1Z    = "notPreviousNot"
opOneAlone2Copilot Op1Hist = "PTLTL.alwaysBeen"
opOneAlone2Copilot Op1O    = "PTLTL.eventuallyPrev"

-- | Return the Copilot representation of a unary logical MTL FRET operator.
opOneMTL2Copilot :: Op1Name -> OrdOp -> Number -> String
opOneMTL2Copilot operator _comparison number =
  opOneAlone2Copilot operator ++ " " ++ show (0 :: Int)
                              ++ " " ++ number2Copilot number
                              ++ " " ++ "clock" ++ " "
                              ++ show (1 :: Int)

-- | Return the Copilot representation of a FRET number.
number2Copilot :: Number -> String
number2Copilot (NumberB   n) = number2Copilot n
number2Copilot (NumberI   n) = number2Copilot n
number2Copilot (NumberInt n) = show n

-- | Return the Copilot representation of a binary logical non-MTL FRET
-- operator.
opTwo2Copilot :: OpTwo -> String
opTwo2Copilot Op2S    = "`since`"
opTwo2Copilot Op2T    = "`triggers`"
opTwo2Copilot Op2V    = "`releases`"
opTwo2Copilot Op2U    = "`until`"
opTwo2Copilot Op2And  = "&&"
opTwo2Copilot Op2Or   = "||"
opTwo2Copilot Op2Xor  = "`xor`"
opTwo2Copilot Op2XNor = "`xnor`"
opTwo2Copilot Op2Impl = "==>"
opTwo2Copilot Op2Equiv = "<==>"
opTwo2Copilot Op2Eq   = "=="
opTwo2Copilot Op2Lt   = "<"
opTwo2Copilot Op2Gt   = ">"
opTwo2Copilot Op2Le   = "<="
opTwo2Copilot Op2Ge   = ">="

-- | Return the Copilot representation of a FRET identifier.
ident2Copilot :: Ident -> String
ident2Copilot (Ident i) = i

-- | Return all identifiers used in a BoolSpec that are not reserved keywords.
boolSpecNames :: BoolSpec -> [String]
boolSpecNames b = case b of
  BoolSpecPar bs               -> boolSpecNames bs
  BoolSpecBPar bs              -> boolSpecNames bs
  BoolSpecIPar bs              -> boolSpecNames bs
  BoolSpecConst _bc            -> []
  BoolSpecSignal (Ident i)     -> [i]
  BoolSpecOp1 _op spec         -> boolSpecNames spec
  BoolSpecOp2 spec1 _op2 spec2 -> boolSpecNames spec1 ++ boolSpecNames spec2
