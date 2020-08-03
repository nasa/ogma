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
