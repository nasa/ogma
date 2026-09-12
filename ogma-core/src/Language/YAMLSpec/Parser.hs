{-# LANGUAGE ScopedTypeVariables #-}
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

-- | Parser for Ogma specs stored in YAML files.
module Language.YAMLSpec.Parser where

-- External imports
import           Control.Monad.Except (ExceptT (..), runExceptT)
import           Data.Aeson           (Value (..))
import           Data.Aeson.Key       (fromString)
import qualified Data.Aeson.KeyMap    as M
import           Data.Bifunctor       (first)
import qualified Data.ByteString      as BS
import           Data.Char            (isSpace)
import           Data.List            (intercalate)
import           Data.Maybe           (maybeToList)
import           Data.Text            (unpack)
import qualified Data.Vector          as V
import qualified Data.Yaml            as Y
import           System.FilePath      (takeBaseName, takeFileName)

-- External imports: ogma-spec
import Data.Either.Extra (mapLeft)
import Data.OgmaSpec     (ExternalVariableDef (..), InternalVariableDef (..),
                          Requirement (..), Spec (..))

-- | Field names of a spec listed in a YAML file.
data YAMLFormat = YAMLFormat
    { specInternalVars          :: Maybe String
    , specInternalVarId         :: String
    , specInternalVarExpr       :: String
    , specInternalVarType       :: Maybe String
    , specExternalVars          :: Maybe String
    , specExternalVarId         :: String
    , specExternalVarType       :: Maybe String
    , specRequirements          :: Maybe String
    , specRequirementId         :: Maybe FieldSource
    , specRequirementDesc       :: Maybe String
    , specRequirementExpr       :: String
    , specRequirementResultType :: Maybe String
    , specRequirementResultExpr :: Maybe String
    }
  deriving (Read)

-- | Source used to populate the value of a field in a spec.
data FieldSource
    = Field String -- ^ A field of the YAML header
    | FileName     -- ^ Filename with extension
    | BaseName     -- ^ Filename without extension
  deriving (Show)

-- | Custom instance to read a 'FieldSource' that allows YAML field names to be
-- written down as plain strings.
instance Read FieldSource where
  readsPrec prec str =
    case lex str of
      [("Field", rest)]    -> first Field <$> readsPrec prec rest
      [("FileName", rest)] -> [(FileName, rest)]
      [("BaseName", rest)] -> [(BaseName, rest)]
      -- If it doesn't match a constructor, we attempt to read a string and
      -- treat it as the name of a YAML field.
      _                    -> first Field <$> readsPrec prec str

-- | Parse a spec from a YAML file.
parseYAMLSpec :: forall a
              .  (String -> IO (Either String a))
              -> YAMLFormat
              -> FilePath
              -> BS.ByteString
              -> IO (Either String (Spec a))
parseYAMLSpec parseExpr yamlFormat filepath bs = runExceptT $ do
  value <- except $ mapLeft Y.prettyPrintParseException $ Y.decodeEither' bs

  let values :: [Value]
      values =
        maybe [] (objectFieldValueList value) (specInternalVars yamlFormat)

      internalVarDef :: Value -> Either String InternalVariableDef
      internalVarDef value = do
        let msg = "internal variable name"
        varId   <- valueToString msg =<<
                     listToEither
                       msg
                       (objectFieldValues (specInternalVarId yamlFormat) value)

        let msg = "internal variable type"
        varType <- maybe
                     (Right "")
                     (\e -> valueToString msg =<<
                              listToEither msg (objectFieldValues e value)
                     )
                     (specInternalVarType yamlFormat)

        let msg = "internal variable expr"
        varExpr <- valueToString msg =<<
                     listToEither
                       msg
                       ( objectFieldValues
                           (specInternalVarExpr yamlFormat)
                           value
                       )

        return $ InternalVariableDef
                   { internalVariableName = varId
                   , internalVariableType = varType
                   , internalVariableExpr = varExpr
                   }

  internalVariableDefs <- except $ mapM internalVarDef values

  let values :: [Value]
      values =
        maybe [] (objectFieldValueList value) (specExternalVars yamlFormat)

      externalVarDef :: Value -> Either String ExternalVariableDef
      externalVarDef value = do

        let msg = "external variable name"
        varId   <- valueToString msg
                      =<< listToEither
                            msg
                            ( objectFieldValues
                                (specExternalVarId yamlFormat)
                                value
                            )

        let msg = "external variable type"
        varType <- maybe
                     (Right "")
                     (\e -> valueToString msg =<<
                              listToEither msg (objectFieldValues e value)
                     )
                     (specExternalVarType yamlFormat)

        return $ ExternalVariableDef
                   { externalVariableName = varId
                   , externalVariableType = varType
                   }

  externalVariableDefs <- except $ mapM externalVarDef values

  let values :: [Value]
      values =
        maybe [value] (objectFieldValueList value) (specRequirements yamlFormat)

      requirementDef value = do
        let msg = "Requirement name"

        -- Handle the case where the requirement ID is the file name, with or
        -- without extension.
        reqId <- case specRequirementId yamlFormat of
          Nothing        -> return ""
          Just FileName  -> return $ takeFileName filepath
          Just BaseName  -> return $ takeBaseName filepath
          Just (Field p) -> except $
            valueToString msg =<< listToEither msg (objectFieldValues p value)

        let msg = "Requirement expression"
        reqExpr <- except $ valueToString msg =<<
                              listToEither
                                msg
                                ( objectFieldValues
                                    (specRequirementExpr yamlFormat)
                                    value
                                )
        reqExpr' <- ExceptT $ parseExpr reqExpr

        let msg = "Requirement description"
        reqDesc <- except $
                     maybe
                       (Right "")
                       (\e -> valueToString msg =<<
                                listToEither msg (objectFieldValues e value)
                       )
                       (specRequirementDesc yamlFormat)
        let reqDesc' = cleanString reqDesc

        let msg = "Requirement result type"
            ty :: Maybe (Either String String)
            ty = (\e -> valueToString msg =<<
                          listToEither msg (objectFieldValues e value)
                 )
             <$> specRequirementResultType yamlFormat
        reqResType <- except $ maybeEither ty

        let msg = "Requirement result expression"
            resultExpr :: Maybe (Either String String)
            resultExpr = (\e -> valueToString msg =<<
                                  listToEither
                                    msg
                                    (objectFieldValues e value)
                         )
                     <$> specRequirementResultExpr yamlFormat

        reqResExpr  <- except $ maybeEither resultExpr
        reqResExpr' <- ExceptT $ case reqResExpr of
                                   Nothing -> return $ Right Nothing
                                   Just x  -> fmap Just <$> parseExpr x

        return $ Requirement
                   { requirementName        = reqId
                   , requirementExpr        = reqExpr'
                   , requirementDescription = reqDesc'
                   , requirementResultType  = reqResType
                   , requirementResultExpr  = reqResExpr'
                   }

  requirements <- mapM requirementDef values

  return $ Spec internalVariableDefs externalVariableDefs requirements

-- * Auxiliary functions

-- ** JSON functions

-- | Convert a string JSON value into a 'String'.
valueToString :: String -> Value -> Either String String
valueToString msg (String x) = Right $ unpack x
valueToString msg _          = Left $
  "The YAML value provided for " ++ msg ++ " does not contain a string"

-- | Object the values associated to a key of an object.
--
-- If the values are an array, it returns the values in the array directly.
objectFieldValueList :: Value -> String -> [Value]
objectFieldValueList (Object o) key =
  case M.lookup (fromString key) o of
    Just (Array arr) -> V.toList arr
    Just v           -> [v]
    Nothing          -> []
objectFieldValueList _ _ = []

-- | Object the values associated to a key of an object.
--
-- If the values are an array, it returns the values in the array directly.
objectFieldValues :: String -> Value -> [Value]
objectFieldValues key (Object o) = maybeToList $ M.lookup (fromString key) o
objectFieldValues _   _          = []

-- ** Either-related auxiliary functions

-- | Convert a string into an Either value.
--
-- Fails if there more or less than one value in the list.
listToEither :: String -> [a] -> Either String a
listToEither _   [x] = Right x
listToEither msg []  = Left $ "Failed to find a value for " ++ msg
listToEither msg _   = Left $ "Unexpectedly found multiple values for " ++ msg

-- | Wrap an 'Either' value in an @ExceptT m@ monad.
except :: Monad m => Either e a -> ExceptT e m a
except = ExceptT . return

-- | Swap the order in a Maybe and an Either monad.
maybeEither :: Maybe (Either a b) -> Either a (Maybe b)
maybeEither Nothing  = Right Nothing
maybeEither (Just e) = fmap Just e

-- ** String-related auxiliary functions

-- | Remove trailing spaces and lines from a multi-line string.
cleanString :: String -> String
cleanString =
      unlines'
    . dropWhile null
    . dropWhileEnd null
    . map strip
    . lines
  where
    strip          = dropWhile isSpace . dropWhileEnd isSpace
    dropWhileEnd x = reverse . dropWhile x . reverse

-- | Concatenate a list of strings into one string, separated by a line ending.
--
-- This variant of 'unlines' does not add a line break at the end of the last
-- line.
unlines' :: [String] -> String
unlines' = intercalate "\n"
