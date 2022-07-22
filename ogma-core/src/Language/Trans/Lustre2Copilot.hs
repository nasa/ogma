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
-- | Transform a Lustre program into a Copilot specification.

module Language.Trans.Lustre2Copilot ( lustre2Copilot ) where

-- Internal imports: Lustre
import qualified Language.Lustre.AbsLustre as Lustre

-- Internal imports: Copilot
import qualified Language.Copilot.AbsCopilot      as Copilot
import qualified Language.Copilot.PrintCopilotNew as Copilot

-- | Return a string with the contents of the Copilot module that implements a
-- Lustre node.
--
-- PRE: The Lustre program does not use any identifiers that exist in Copilot.
-- All identifiers used are valid C99 identifiers.
lustre2Copilot :: Lustre.Program
               -> Either String String
lustre2Copilot program =
  (unlines . map Copilot.printTree) <$> lustre2CopilotProgram program

lustre2CopilotProgram :: Lustre.Program -> Either String [Copilot.Def]
lustre2CopilotProgram (Lustre.Program p) = lustre2CopilotProgramEntries p

lustre2CopilotProgramEntries :: [Lustre.ProgramEntry] -> Either String [Copilot.Def]
lustre2CopilotProgramEntries = mapM lustre2CopilotProgramEntry

lustre2CopilotProgramEntry :: Lustre.ProgramEntry -> Either String Copilot.Def
lustre2CopilotProgramEntry (Lustre.ProgramEntryNode node)      = lustre2CopilotNode node
lustre2CopilotProgramEntry (Lustre.ProgramEntryTypeDef  _node) = Left "cannot translate Lustre type definitions."
lustre2CopilotProgramEntry (Lustre.ProgramEntryConstant _node) = Left "cannot translate Lustre constants."
lustre2CopilotProgramEntry (Lustre.ProgramEntryFunction _node) = Left "cannot translate Lustre functions."

lustre2CopilotNode :: Lustre.Node -> Either String Copilot.Def
lustre2CopilotNode
  (Lustre.Node id_ varDeclList1 varDeclList2 _varDeclBlockOpt letSeq _semicolon) =
  Copilot.MkDef <$> Right (lustre2CopilotId id_)
                <*> lustreVarDeclListOpt2CopilotIdentList varDeclList1
                <*> ((Copilot.StreamIdent . head) <$> lustreVarDeclListOpt2CopilotIdentList varDeclList2)
                <*> (Copilot.LocalDefJust <$> lustre2CopilotLetSeq' letSeq)

lustreVarDeclListOpt2CopilotIdentList :: Lustre.VarDeclListOpt -> Either String [Copilot.Ident]
lustreVarDeclListOpt2CopilotIdentList Lustre.VarDeclListOpt0     = pure []
lustreVarDeclListOpt2CopilotIdentList (Lustre.VarDeclListOpt1 e) = lustreVarDeclList2CopilotIdentList e

lustreVarDeclList2CopilotIdentList :: Lustre.VarDeclList -> Either String [Copilot.Ident]
lustreVarDeclList2CopilotIdentList (Lustre.VarDeclList1 e)    = lustreVarDeclGroup2CopilotIdentList e
lustreVarDeclList2CopilotIdentList (Lustre.VarDeclListN en e) = (++) <$> lustreVarDeclList2CopilotIdentList en
                                                                     <*> lustreVarDeclGroup2CopilotIdentList e

lustreVarDeclGroup2CopilotIdentList :: Lustre.VarDeclGroup
                                    -> Either String [Copilot.Ident]
lustreVarDeclGroup2CopilotIdentList (Lustre.VarDeclGroup varSeq _type) =
  lustreEIDSeq2CopilotIdentList varSeq

lustreEIDSeq2CopilotIdentList :: Lustre.EIDSeq -> Either String [Copilot.Ident]
lustreEIDSeq2CopilotIdentList (Lustre.EIDSeq1Base eid) = (:[]) <$> lustreEID2CopilotIdent eid
lustreEIDSeq2CopilotIdentList (Lustre.EIDSeq1Req eids eid) = (++) <$> lustreEIDSeq2CopilotIdentList eids
                                                                  <*> ((:[]) <$> lustreEID2CopilotIdent eid)

lustreEID2CopilotIdent :: Lustre.EID -> Either String Copilot.Ident
lustreEID2CopilotIdent (Lustre.EIDId eid) = Right $ lustre2CopilotId eid
lustreEID2CopilotIdent e = Left $ "cannot convert eids like: " ++ show e

lustre2CopilotLetSeq' :: Lustre.LetSeq -> Either String Copilot.DefList
lustre2CopilotLetSeq' Lustre.LetSeq0 = pure Copilot.DefList0
lustre2CopilotLetSeq' (Lustre.LetSeq1 ls lsI) =
  Copilot.DefListN <$> (lustre2CopilotLetSeq' ls)
                   <*> (lustre2CopilotLetSeqItem' lsI)

lustre2CopilotLetSeqItem' :: Lustre.LetSeqItem -> Either String Copilot.Def
lustre2CopilotLetSeqItem' (Lustre.LetSeqItemEquation equation) = lustre2CopilotEquation' equation
lustre2CopilotLetSeqItem' (Lustre.LetSeqItemProperty _)   = Left "local property definitions not supported."
lustre2CopilotLetSeqItem' (Lustre.LetSeqItemAssertion _)  = Left "local assertion definitions not supported."
lustre2CopilotLetSeqItem' (Lustre.LetSeqItemMain _)       = Left "local main definitions not supported."
lustre2CopilotLetSeqItem' (Lustre.LetSeqItemRealInputs _) = Left "local real input definitions not supported."
lustre2CopilotLetSeqItem' (Lustre.LetSeqItemIVC _)        = Left "local IVC definitions not supported."

lustre2CopilotEquation' :: Lustre.Equation -> Either String Copilot.Def
lustre2CopilotEquation' (Lustre.Equation eqLHS expr)
  = Copilot.MkDef <$> (lustre2CopilotLHS eqLHS)
                  <*> pure []
                  <*> (lustre2CopilotExpr expr)
                  <*> pure Copilot.LocalDefNothing

lustre2CopilotLHS :: Lustre.EqLHS -> Either String Copilot.Ident
lustre2CopilotLHS (Lustre.EqLHS1 (Lustre.Lhs (Lustre.EIDList1 (Lustre.EIDId eid)))) =
  pure $ lustre2CopilotId eid
lustre2CopilotLHS (Lustre.EqLHS1 (Lustre.Lhs Lustre.EIDList0)) =
  Left "equations with no ID on the left-hand side not supported."
lustre2CopilotLHS (Lustre.EqLHS1 (Lustre.Lhs (Lustre.EIDListN _ _))) =
  Left "equations with multiple IDs on the left-hand side not supported."
lustre2CopilotLHS (Lustre.EqLHS1 (Lustre.Lhs (Lustre.EIDList1 (Lustre.EIDArray _ _)))) =
  Left "equations with arrays on the left-hand side not supported."
lustre2CopilotLHS (Lustre.EqLHS1 (Lustre.Lhs (Lustre.EIDList1 (Lustre.EIDRecord _ _)))) =
  Left "equations with records on the left-hand side not supported."
lustre2CopilotLHS (Lustre.EqLHS1 (Lustre.Lhs (Lustre.EIDListN1 _ _))) =
  Left "equations multiple IDs on the left-hand side not supported."
lustre2CopilotLHS Lustre.EqLHSPar =
  Left "equations with parenthesis on the left-hand side not supported."
lustre2CopilotLHS (Lustre.EqLHSLhs _) =
  Left "equations with this kind of element on the left-hand side not supported."

-- lustre2CopilotLetSeq :: Lustre.LetSeq -> [Copilot.Stream]
-- lustre2CopilotLetSeq Lustre.LetSeq0 = []
-- lustre2CopilotLetSeq (Lustre.LetSeq1 ls lsI) =
--   lustre2CopilotLetSeq ls ++ [lustre2CopilotLetSeqItem lsI]

-- lustre2CopilotLetSeqItem :: Lustre.LetSeqItem -> Copilot.Stream
-- lustre2CopilotLetSeqItem (Lustre.LetSeqItemEquation eq) =
--   lustre2CopilotEquation eq

-- lustre2CopilotEquation :: Lustre.Equation -> Copilot.Stream
-- lustre2CopilotEquation (Lustre.Equation _eqLHS expr)
--   = lustre2CopilotExpr expr

lustre2CopilotExpr :: Lustre.Expr -> Either String Copilot.Stream
lustre2CopilotExpr e = Copilot.StreamPar <$> case e of
  (Lustre.IdExpr id_)                                          -> Right $ Copilot.StreamIdent (lustre2CopilotId id_)
  (Lustre.IntExpr numConst)                                    -> Right $ Copilot.ConstStream (lustre2CopilotInt numConst)
  (Lustre.BoolExpr boolConst)                                  -> Right $ Copilot.ConstStream (lustre2CopilotBool boolConst)
  (Lustre.RealExpr realConst)                                  -> Right $ Copilot.ConstStream (lustre2CopilotReal realConst)
  (Lustre.NotExpr  expr)                                       -> Copilot.StreamOP1 <$> pure (Copilot.OPOne "not") <*> (lustre2CopilotExpr expr)
  (Lustre.NegateExpr expr)                                     -> Copilot.StreamOP1 <$> pure (Copilot.OPOne "-")   <*> (lustre2CopilotExpr expr) -- Violates the grammar
  (Lustre.CastExpr Lustre.CastTypeReal expr)                   -> Copilot.StreamOP1 <$> pure (Copilot.OPOne "cast") <*> (lustre2CopilotExpr expr)
  (Lustre.CastExpr Lustre.CastTypeInt _expr)                   -> Left "casting from real to int is not yet implemented."
  (Lustre.CondactExpr _expr)                                   -> Left "condact is not yet implemented."
  (Lustre.RecordExpr _id _assignList)                          -> Left "Copilot does not support record updates."
  (Lustre.ArrayExpr _exprList)                                 -> Left "Copilot does not support array updates."
  (Lustre.TupleExpr (Lustre.ExprList1Base expr))               -> lustre2CopilotExpr expr
  (Lustre.TupleExpr exprList)                                  -> Left $ "Copilot does not support tuple updates: " ++ show exprList ++ "."
  (Lustre.IfThenElseExpr ifE thenE elseE)                      -> Copilot.StreamOP3 (Copilot.OPThree "mux") <$> (lustre2CopilotExpr ifE) <*> (lustre2CopilotExpr thenE) <*> (lustre2CopilotExpr elseE)
  (Lustre.PreExpr expr)                                        -> Copilot.StreamAppend <$> pure (Copilot.MkValueList [Copilot.ValueUID (Copilot.Ident "undefined") []]) <*> (lustre2CopilotExpr expr)
  (Lustre.AppExpression expr (Lustre.RecordAccessExpr id_))    -> Copilot.StreamStruct <$> (lustre2CopilotExpr expr) <*> pure (lustre2CopilotId id_)
  (Lustre.AppExpression _expr (Lustre.RecordUpdateExpr _ _))   -> Left "Copilot does not support record updates."
  (Lustre.AppExpression expr1 (Lustre.ArrayAccessExpr expr2))  -> Copilot.StreamOP2 <$> (lustre2CopilotExpr expr1) <*> pure (Copilot.OPTwo ".!!") <*> (lustre2CopilotExpr expr2)
  (Lustre.AppExpression _expr (Lustre.ArrayUpdateExpr _ _ ))   -> Left "Copilot does not support array updates."

  (Lustre.AppExpression expr1 (Lustre.BinaryExpr Lustre.Op2Delay (Lustre.PreExpr expr2))) ->
    Copilot.StreamAppend
      <$> (Copilot.MkValueList <$> (:[]) <$> lustre2CopilotValue expr1)
      <*> (lustre2CopilotExpr expr2)
  (Lustre.AppExpression expr1 (Lustre.BinaryExpr op expr2))    -> Copilot.StreamOP2 <$> (lustre2CopilotExpr expr1) <*> (lustre2CopilotBinOp op) <*> (lustre2CopilotExpr expr2)
  (Lustre.AppExpression _expr (Lustre.CallExpr _))             -> Left "Copilot does not support arbitrary function calls."

lustre2CopilotValue :: Lustre.Expr -> Either String Copilot.Value
lustre2CopilotValue (Lustre.IdExpr id_)         = Right $ lustre2CopilotIdentValue $ lustre2CopilotId id_
lustre2CopilotValue (Lustre.IntExpr numConst)   = Right $ lustre2CopilotInt numConst
lustre2CopilotValue (Lustre.BoolExpr boolConst) = Right $ lustre2CopilotBool boolConst
lustre2CopilotValue (Lustre.RealExpr realConst) = Right $ lustre2CopilotReal realConst
lustre2CopilotValue e                           = Left $ "conversion of expression to copilot value not supported " ++ show e

lustre2CopilotIdentValue :: Copilot.Ident -> Copilot.Value
lustre2CopilotIdentValue id_ = Copilot.ValueUID id_ []

lustre2CopilotInt :: Lustre.NumConst -> Copilot.Value
lustre2CopilotInt (Lustre.NumConst (Lustre.Bound s)) =
  Copilot.ValueInt (Copilot.VINT s)

lustre2CopilotBool :: Lustre.BoolConst -> Copilot.Value
lustre2CopilotBool Lustre.BoolFalse = Copilot.ValueBool (Copilot.VBOOL "false")
lustre2CopilotBool Lustre.BoolTrue  = Copilot.ValueBool (Copilot.VBOOL "true")

lustre2CopilotReal :: Lustre.RealConst -> Copilot.Value
lustre2CopilotReal (Lustre.RealConst d) =
  Copilot.ValueFloat (Copilot.VFLOAT (show d))

lustre2CopilotReal (Lustre.RealNeg (Lustre.NegDouble s)) =
  Copilot.ValueFloat (Copilot.VFLOAT s)

lustre2CopilotId :: Lustre.ID -> Copilot.Ident
lustre2CopilotId (Lustre.ID s) = Copilot.Ident s

lustre2CopilotBinOp :: Lustre.OP -> Either String Copilot.OPTwo
lustre2CopilotBinOp Lustre.Op2And   = Right $ Copilot.OPTwo "&&"
lustre2CopilotBinOp Lustre.Op2Or    = Right $ Copilot.OPTwo "||"
lustre2CopilotBinOp Lustre.Op2XOr   = Right $ Copilot.OPTwo "`xor`"
lustre2CopilotBinOp Lustre.Op2Delay = Left "delay not yet implemented."
lustre2CopilotBinOp Lustre.Op2Impl  = Right $ Copilot.OPTwo "==>"
lustre2CopilotBinOp Lustre.Op2Eq    = Right $ Copilot.OPTwo "=="
lustre2CopilotBinOp Lustre.Op2NEq   = Right $ Copilot.OPTwo "/="
lustre2CopilotBinOp Lustre.Op2LT    = Right $ Copilot.OPTwo "<"
lustre2CopilotBinOp Lustre.Op2LE    = Right $ Copilot.OPTwo "<="
lustre2CopilotBinOp Lustre.Op2GT    = Right $ Copilot.OPTwo ">"
lustre2CopilotBinOp Lustre.Op2GE    = Right $ Copilot.OPTwo ">="
lustre2CopilotBinOp Lustre.Op2Div   = Right $ Copilot.OPTwo "/"
lustre2CopilotBinOp Lustre.Op2Mod   = Right $ Copilot.OPTwo "`mod`"
lustre2CopilotBinOp Lustre.Op2Min   = Right $ Copilot.OPTwo "-"
lustre2CopilotBinOp Lustre.Op2Plus  = Right $ Copilot.OPTwo "+"
lustre2CopilotBinOp Lustre.Op2Mult  = Right $ Copilot.OPTwo "*"
