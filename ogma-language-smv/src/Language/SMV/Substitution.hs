module Language.SMV.Substitution where

import qualified Language.SMV.AbsSMV as SMV

substituteBoolExpr subs e = foldr subBS e subs

-- Substitute name x if it matches the old name oName
subsName (oName, nName) x = if x == oName then nName else x

-- Substitute a name in all identifiers in a boolean expression
subBS sub' = mapBoolSpecIdent (subsName sub')

-- Traverse a boolean expression applying a function to all identifiers
mapBoolSpecIdent :: (String -> String) -> SMV.BoolSpec -> SMV.BoolSpec
mapBoolSpecIdent f boolSpec =
  case boolSpec of
    SMV.BoolSpecSignal (SMV.Ident i) -> SMV.BoolSpecSignal (SMV.Ident (f i))

    SMV.BoolSpecConst bc -> SMV.BoolSpecConst bc

    SMV.BoolSpecNum e -> SMV.BoolSpecNum (mapNumExprIdent f e)

    SMV.BoolSpecCmp spec1 op2 spec2 -> SMV.BoolSpecCmp
                                         (mapBoolSpecIdent f spec1) op2
                                         (mapBoolSpecIdent f spec2)

    SMV.BoolSpecNeg spec -> SMV.BoolSpecNeg (mapBoolSpecIdent f spec)

    SMV.BoolSpecAnd spec1 spec2 -> SMV.BoolSpecAnd
                                         (mapBoolSpecIdent f spec1)
                                         (mapBoolSpecIdent f spec2)

    SMV.BoolSpecOr spec1 spec2 -> SMV.BoolSpecOr
                                        (mapBoolSpecIdent f spec1)
                                        (mapBoolSpecIdent f spec2)

    SMV.BoolSpecXor spec1 spec2 -> SMV.BoolSpecXor
                                         (mapBoolSpecIdent f spec1)
                                         (mapBoolSpecIdent f spec2)

    SMV.BoolSpecImplies spec1 spec2 -> SMV.BoolSpecImplies
                                             (mapBoolSpecIdent f spec1)
                                             (mapBoolSpecIdent f spec2)

    SMV.BoolSpecEquivs spec1 spec2 -> SMV.BoolSpecEquivs
                                            (mapBoolSpecIdent f spec1)
                                            (mapBoolSpecIdent f spec2)

    SMV.BoolSpecOp1 op spec -> SMV.BoolSpecOp1 op (mapBoolSpecIdent f spec)

    SMV.BoolSpecOp2 spec1 op2 spec2 -> SMV.BoolSpecOp2
                                         (mapBoolSpecIdent f spec1) op2
                                         (mapBoolSpecIdent f spec2)

-- Traverse a numeric expression applying a function to all identifiers
mapNumExprIdent :: (String -> String) -> SMV.NumExpr -> SMV.NumExpr
mapNumExprIdent f numExpr =
  case numExpr of
    SMV.NumId (SMV.Ident i)    -> SMV.NumId (SMV.Ident (f i))
    SMV.NumConstI c            -> SMV.NumConstI c
    SMV.NumConstD c            -> SMV.NumConstD c
    SMV.NumAdd expr1 op expr2  -> SMV.NumAdd
                                        (mapNumExprIdent f expr1)
                                        op
                                        (mapNumExprIdent f expr2)
    SMV.NumMult expr1 op expr2 -> SMV.NumMult
                                        (mapNumExprIdent f expr1)
                                        op
                                        (mapNumExprIdent f expr2)
