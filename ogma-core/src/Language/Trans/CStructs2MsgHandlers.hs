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
