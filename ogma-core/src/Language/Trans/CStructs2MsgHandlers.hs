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
-- | Generate C methods that process message dealing with the structs
-- defined in a header file.
--
-- This module contains the pure conversion from CStructs into C code.
-- Normally, this module would be implemented as a conversion between C ASTs,
-- but we want to add comments to the generated code, which are not
-- representable in the abstract syntax tree.
module Language.Trans.CStructs2MsgHandlers where

-- Internal imports: C AST representation.
import qualified Language.C.AbsC as C ( TranslationUnit (MkTranslationUnit) )

-- Internal imports: Copilot's own CStruct representation.
import Language.Copilot.CStruct ( CStruct (cStructName) )

import Language.Trans.CStruct2CopilotStruct ( camelCaseTypeName, mkCStruct )

-- | Generate a C methods that process message dealing with the structs
-- defined in a header file.
cstructs2MsgHandlers :: C.TranslationUnit -> Either String String
cstructs2MsgHandlers (C.MkTranslationUnit gs) =
  unlines <$> mapM (fmap cstruct2MsgHandler . mkCStruct) gs

-- | Generate a C method that processes one message dealing with one
-- kind of struct.
cstruct2MsgHandler :: CStruct -> String
cstruct2MsgHandler cstruct = unlines
    [ nameCStruct ++ " " ++ nameLocalVar ++ ";"
    , ""
    , "/**"
    , "* Make ICAROUS data available to Copilot and run monitors."
    , "*/"
    , "void COPILOT_Process" ++ nameVar ++ "Monitor(void)"
    , "{"
    , "  " ++ nameCStruct ++ "* msg;"
    , "  msg = (" ++ nameCStruct  ++ "*) COPILOTMsgPtr;"
    , "  " ++ nameLocalVar ++ " = *msg;"
    , ""
    , "  // Run all copilot monitors."
    , "  step();"
    , "}"
    ]
  where
    nameCStruct  = cStructName cstruct
    nameVar      = camelCaseTypeName nameCStruct
    nameLocalVar = 'm' : 'y' : camelCaseTypeName nameCStruct
