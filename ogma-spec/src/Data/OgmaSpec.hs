-- Copyright 2024 United States Government as represented by the Administrator
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
-- | Abstract representation of an Ogma specification.
module Data.OgmaSpec where

-- | Abstract representation of an Ogma specification.
data Spec a = Spec
    { internalVariables :: [ InternalVariableDef ]
    , externalVariables :: [ ExternalVariableDef ]
    , requirements      :: [ Requirement a ]
    }
  deriving (Show)

-- | Internal variable definition, with a given name, its type and definining
-- expression.
data InternalVariableDef = InternalVariableDef
    { internalVariableName    :: String
    , internalVariableType    :: String
    , internalVariableExpr    :: String
    }
  deriving (Show)

-- | External variable definition, with a given name and type.
--
-- The value of external variables is assigned outside Copilot, so they have no
-- defining expression in this type.
data ExternalVariableDef = ExternalVariableDef
    { externalVariableName :: String
    , externalVariableType :: String
    }
  deriving (Show)

-- | Requirement with a given name and a boolean expression.
data Requirement a = Requirement
    { requirementName        :: String
    , requirementExpr        :: a
    , requirementDescription :: String
    , requirementResultType  :: Maybe String
    , requirementResultExpr  :: Maybe a
    }
  deriving (Show)
