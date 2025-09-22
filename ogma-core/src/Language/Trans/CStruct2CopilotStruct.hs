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
-- | Copilot's struct representation of C Structs and creation from C's AST.
module Language.Trans.CStruct2CopilotStruct
    (
      -- * Constructors
      mkCStruct

      -- * Convert C type names to valid Copilot names
    , camelCaseTypeName
    )
  where

-- External imports
import Data.Char ( toUpper )

-- External imports: Copilot C Struct representation
import Language.Copilot.CStruct ( CField (CArray, CPlain), CStruct(..) )

-- Internal imports
import qualified Language.C.AbsC as C

-- | Convert a top-level struct declaration into a CStruct
mkCStruct :: C.ExternalDeclaration -> Either String CStruct
mkCStruct (C.MkExternalDeclarationFunctionDefinition _) = Left "C files must contain struct definitions only."
mkCStruct (C.MkExternalDeclarationDeclaration (C.MkDeclaration specifiers initDecl)) =
  case specifiers of
    C.DeclarationSpecifiers (C.MkDeclarationSpecifierStorageClass C.MkStorageClassSpecifierTypedef) s ->
      let [C.MkDeclarationSpecifierTypeSpecifier (C.MkTypeSpecifierStructOrUnion (C.MkStructOrUnionSpecifierWithFields C.MkStructOrUnionStruct _structName u))] = s
          (C.MkInitDeclarationListOptJust [C.MkInitDeclaratorUninitialized (C.MkDeclarator C.MkPointerOptNothing (C.MkDirectDeclaratorIdentifier (C.Identifier t)))]) = initDecl
          name = Right t
          fields = mapM buildCField u
      in CStruct <$> name <*> fields
    _ -> Left "C files must contain struct definitions only."

-- -- | Convert a declaration within a struct into a field declaration.
buildCField :: C.StructDeclaration -> Either String CField
buildCField (C.MkStructDeclaration field name)
    | fieldLength > 0 = CArray <$> fieldType <*> fieldName <*> pure fieldLength
    | otherwise       = CPlain <$> fieldType <*> fieldName
  where
    fieldType   = extractFieldType (head field)
    fieldName   = extractFieldName (head name)
    fieldLength = extractFieldLength (head name)

-- | Extract the type of a field from a type specification.
extractFieldType :: C.SpecifierQualifier -> Either String String
extractFieldType (C.MkSpecifierQualifierTypeSpecifier t) = Right $ showTypeSpecifier t
extractFieldType (C.MkSpecifierQualifierTypeQualifier _) = Left "type qualifiers."

-- | String representing a known type.
showTypeSpecifier :: C.TypeSpecifier -> String
showTypeSpecifier C.MkTypeSpecifierFloat  = "float"
showTypeSpecifier C.MkTypeSpecifierDouble = "double"
showTypeSpecifier C.MkTypeSpecifierUInt8  = "uint8_t"
showTypeSpecifier C.MkTypeSpecifierUInt16 = "uint16_t"
showTypeSpecifier C.MkTypeSpecifierUInt32 = "uint32_t"
showTypeSpecifier C.MkTypeSpecifierUInt64 = "uint64_t"
showTypeSpecifier C.MkTypeSpecifierInt8   = "int8_t"
showTypeSpecifier C.MkTypeSpecifierInt16  = "int16_t"
showTypeSpecifier C.MkTypeSpecifierInt32  = "int32_t"
showTypeSpecifier C.MkTypeSpecifierInt64  = "int64_t"
showTypeSpecifier C.MkTypeSpecifierInt    = "int"

-- -- | Extract the name of a field from a struct declarator.
extractFieldName :: Read n => C.StructDeclarator -> Either String n
extractFieldName (C.MkStructDeclaratorDeclarator (C.MkDeclarator C.MkPointerOptNothing (C.MkDirectDeclaratorIdentifier (C.Identifier d)))) = Right $ read $ show d
extractFieldName (C.MkStructDeclaratorDeclarator
                    (C.MkDeclarator
                      C.MkPointerOptNothing
                      (C.MkDirectDeclaratorConstantExpressionOpt
                         (C.MkDirectDeclaratorIdentifier (C.Identifier i))
                         _arrayLength
                      )
                    )
                  ) = Right $ read $ show i
extractFieldName _ = Left $ "only struct declarations that are IDs without a"
                        ++  " pointer, or plain arrays without a pointer, are"
                        ++  " supported."
--
-- -- | Extract the length of an array field from a struct declarator.
extractFieldLength :: C.StructDeclarator -> Integer
extractFieldLength (C.MkStructDeclaratorDeclarator
                     (C.MkDeclarator
                       C.MkPointerOptNothing
                       (C.MkDirectDeclaratorConstantExpressionOpt
                          _varIdent
                          (C.MkConditionalExpressionJust
                            (C.MkConstantExpression
                              (C.Expression12
                                (C.MkCastExpression1
                                   (C.MkUnaryExpressionPostfix
                                      (C.MkPostfixExpression1
                                         (C.MkPrimaryExpressionIdentifier (C.Identifier _n))
                                      )
                                   )
                                )
                              )
                            )
                          )
                       )
                     ) ) = 99
extractFieldLength (C.MkStructDeclaratorDeclarator
                     (C.MkDeclarator
                       C.MkPointerOptNothing
                       (C.MkDirectDeclaratorConstantExpressionOpt
                         _varIdent
                         (C.MkConditionalExpressionJust
                           (C.MkConstantExpression
                             (C.Expression12
                               (C.MkCastExpression1
                                  (C.MkUnaryExpressionPostfix
                                     (C.MkPostfixExpression1
                                       (C.MkPrimaryExpressionConstant (C.MkConstantInteger (C.IntegerConstant i))
                                     )
                                   )
                                 )
                               )
                             )
                           )
                         )
                       )
                     )
                   ) = read i
extractFieldLength _ = 0

--
-- | Convert a 'String' to camel case, also eliminating the @_t@ at the end if
-- present.
camelCaseTypeName :: String -> String
camelCaseTypeName []     = []
camelCaseTypeName (x:xs) = toUpper x : camelCaseTypeName' xs
  where
    camelCaseTypeName' :: String -> String
    camelCaseTypeName' []   = []
    camelCaseTypeName' "_t" = []
    camelCaseTypeName' ('_':y:ys) = toUpper y : camelCaseTypeName' ys
    camelCaseTypeName' (y:ys) = y : camelCaseTypeName' ys
