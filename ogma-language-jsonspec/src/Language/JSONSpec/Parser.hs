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

-- | Parser for Ogma specs stored in JSON files.
module Language.JSONSpec.Parser where

-- External imports
import           Control.Monad.Except  (ExceptT (..), runExceptT)
import           Data.Aeson            (Value (..))
import           Data.Bifunctor        (first)
import           Data.JSONPath.Execute (executeJSONPath)
import           Data.JSONPath.Parser  (jsonPath)
import           Data.JSONPath.Types   (JSONPathElement(..))
import           Data.Text             (pack, unpack)
import qualified Data.Text             as T
import           System.FilePath       (takeBaseName, takeFileName)
import           Text.Megaparsec       (eof, errorBundlePretty, parse)

-- External imports: ogma-spec
import Data.OgmaSpec (ExternalVariableDef (..), InternalVariableDef (..),
                      Requirement (..), Spec (..))

data JSONFormat = JSONFormat
    { specInternalVars          :: Maybe String
    , specInternalVarId         :: String
    , specInternalVarExpr       :: String
    , specInternalVarType       :: Maybe String
    , specExternalVars          :: Maybe String
    , specExternalVarId         :: String
    , specExternalVarType       :: Maybe String
    , specRequirements          :: String
    , specRequirementId         :: FieldSource
    , specRequirementDesc       :: Maybe String
    , specRequirementExpr       :: String
    , specRequirementResultType :: Maybe String
    , specRequirementResultExpr :: Maybe String
    }
  deriving (Read)

-- | Source used to populate the value of a field in a spec.
data FieldSource
    = JSONPath String -- ^ JSON path
    | FileName        -- ^ Filename with extension
    | BaseName        -- ^ Filename without extension
  deriving (Show)

-- | Custom instance to read a 'FieldSource' that allows JSON paths to be
-- written down as plain strings.
instance Read FieldSource where
  readsPrec prec str =
    case lex str of
      [("JSONPath", rest)] -> first JSONPath <$> readsPrec prec rest
      [("FileName", rest)] -> [(FileName, rest)]
      [("BaseName", rest)] -> [(BaseName, rest)]
      -- If it doesn't match a constructor, we attempt to read a string and
      -- treat it as a JSONPath.
      _                    -> first JSONPath <$> readsPrec prec str

data JSONFormatInternal = JSONFormatInternal
  { jfiInternalVars          :: Maybe [JSONPathElement]
  , jfiInternalVarId         :: [JSONPathElement]
  , jfiInternalVarExpr       :: [JSONPathElement]
  , jfiInternalVarType       :: Maybe [JSONPathElement]
  , jfiExternalVars          :: Maybe [JSONPathElement]
  , jfiExternalVarId         :: [JSONPathElement]
  , jfiExternalVarType       :: Maybe [JSONPathElement]
  , jfiRequirements          :: [JSONPathElement]
  , jfiRequirementId         :: FieldSourceInternal
  , jfiRequirementDesc       :: Maybe [JSONPathElement]
  , jfiRequirementExpr       :: [JSONPathElement]
  , jfiRequirementResultType :: Maybe [JSONPathElement]
  , jfiRequirementResultExpr :: Maybe [JSONPathElement]
  }

-- | Internal representation of the source used to populate the value of a
-- field in a spec.
data FieldSourceInternal
    = FSIJSONPath [JSONPathElement] -- ^ JSON path
    | FSIFileName                   -- ^ Filename with extension
    | FSIBaseName                   -- ^ Filename without extension
  deriving (Show)

parseJSONFormat :: JSONFormat -> Either String JSONFormatInternal
parseJSONFormat jsonFormat = do
  jfi2 <- showErrorsM $
            parseJSONPath . pack <$> specInternalVars jsonFormat
  jfi3 <- showErrors $
            parseJSONPath $ pack $ specInternalVarId jsonFormat
  jfi4 <- showErrors $
            parseJSONPath $ pack $ specInternalVarExpr jsonFormat
  jfi5 <- showErrorsM $
            parseJSONPath . pack <$> specInternalVarType jsonFormat
  jfi6 <- showErrorsM $
            parseJSONPath . pack <$> specExternalVars jsonFormat
  jfi7 <- showErrors $
            parseJSONPath $ pack $ specExternalVarId jsonFormat
  jfi8 <- showErrorsM $
            parseJSONPath . pack <$> specExternalVarType jsonFormat
  jfi9 <- showErrors $
            parseJSONPath $ pack $ specRequirements jsonFormat

  -- Handle the case where the requirement ID is the file name, with or without
  -- extension.
  jfi10 <- case specRequirementId jsonFormat of
    FileName   -> return FSIFileName
    BaseName   -> return FSIBaseName
    JSONPath p -> showErrors $ fmap FSIJSONPath $ parseJSONPath $ pack p

  jfi11 <- showErrorsM $
             parseJSONPath . pack <$> specRequirementDesc jsonFormat
  jfi12 <- showErrors $
             parseJSONPath $ pack $ specRequirementExpr jsonFormat
  jfi13 <- showErrorsM $
             parseJSONPath . pack <$> specRequirementResultType jsonFormat
  jfi14 <- showErrorsM $
             parseJSONPath . pack <$> specRequirementResultExpr jsonFormat
  return $ JSONFormatInternal
             { jfiInternalVars          = jfi2
             , jfiInternalVarId         = jfi3
             , jfiInternalVarExpr       = jfi4
             , jfiInternalVarType       = jfi5
             , jfiExternalVars          = jfi6
             , jfiExternalVarId         = jfi7
             , jfiExternalVarType       = jfi8
             , jfiRequirements          = jfi9
             , jfiRequirementId         = jfi10
             , jfiRequirementDesc       = jfi11
             , jfiRequirementExpr       = jfi12
             , jfiRequirementResultType = jfi13
             , jfiRequirementResultExpr = jfi14
             }

parseJSONSpec :: (String -> IO (Either String a))
              -> JSONFormat
              -> FilePath
              -> Value
              -> IO (Either String (Spec a))
parseJSONSpec parseExpr jsonFormat filepath specValue = runExceptT $ do
  jsonFormatInternal <- except $ parseJSONFormat jsonFormat

  let internalValues :: [Value]
      internalValues =
        maybe [] (`executeJSONPath` specValue) (jfiInternalVars jsonFormatInternal)

      internalVarDef :: Value -> Either String InternalVariableDef
      internalVarDef value = do
        let varNameMsg = "internal variable name"
        varId <- valueToString varNameMsg =<<
                   listToEither
                     varNameMsg
                     ( executeJSONPath
                         (jfiInternalVarId jsonFormatInternal)
                         value
                     )

        let varTypeMsg = "internal variable type"
        varType <- maybe
                     (Right "")
                     (\e -> valueToString varTypeMsg =<<
                              listToEither varTypeMsg (executeJSONPath e value)
                     )
                     (jfiInternalVarType jsonFormatInternal)

        let varExprMsg = "internal variable expr"
        varExpr <- valueToString varExprMsg =<<
                     listToEither
                       varExprMsg
                       ( executeJSONPath
                           (jfiInternalVarExpr jsonFormatInternal)
                           value
                       )

        return $ InternalVariableDef
                   { internalVariableName = varId
                   , internalVariableType = varType
                   , internalVariableExpr = varExpr
                   }

  internalVariableDefs <- except $ mapM internalVarDef internalValues

  let externalValues :: [Value]
      externalValues =
        maybe [] (`executeJSONPath` specValue) (jfiExternalVars jsonFormatInternal)

      externalVarDef :: Value -> Either String ExternalVariableDef
      externalVarDef value = do

        let varNameMsg = "external variable name"
        varId <- valueToString varNameMsg =<<
                   listToEither
                     varNameMsg
                     ( executeJSONPath
                         (jfiExternalVarId jsonFormatInternal)
                         value
                     )

        let varTypeMsg = "external variable type"
        varType <-
          maybe
            (Right "")
            (\e -> valueToString varTypeMsg =<<
                     listToEither varTypeMsg (executeJSONPath e value)
            )
            (jfiExternalVarType jsonFormatInternal)

        return $ ExternalVariableDef
                   { externalVariableName = varId
                   , externalVariableType = varType
                   }

  externalVariableDefs <- except $ mapM externalVarDef externalValues

  let requirementValues :: [Value]
      requirementValues = executeJSONPath (jfiRequirements jsonFormatInternal) specValue

      -- requirementDef :: Value -> Either String (Requirement a)
      requirementDef value = do
        let reqNameMsg = "Requirement name"

        -- Handle the case where the requirement ID is the file name, with or
        -- without extension.
        reqId <- case jfiRequirementId jsonFormatInternal of
          FSIFileName   -> return $ takeFileName filepath
          FSIBaseName   -> return $ takeBaseName filepath
          FSIJSONPath p -> except $
            valueToString reqNameMsg =<< listToEither reqNameMsg (executeJSONPath p value)

        let reqExprMsg = "Requirement expression"
        reqExpr <- except $ valueToString reqExprMsg =<<
                              listToEither
                                reqExprMsg
                                ( executeJSONPath
                                    (jfiRequirementExpr jsonFormatInternal)
                                    value
                                )
        reqExpr' <- ExceptT $ parseExpr reqExpr

        let reqDescMsg = "Requirement description"
        reqDesc <- except $ maybe
                     (Right "")
                     (\e -> valueToString reqDescMsg =<<
                              listToEither reqDescMsg (executeJSONPath e value)
                     )
                     (jfiRequirementDesc jsonFormatInternal)

        let reqResTypeMsg = "Requirement result type"
            ty :: Maybe (Either String String)
            ty = (\e -> valueToString reqResTypeMsg =<<
                          listToEither reqResTypeMsg (executeJSONPath e value)
                 )
             <$> jfiRequirementResultType jsonFormatInternal
        reqResType <- except $ maybeEither ty

        let reqResExprMsg = "Requirement result expression"
            resultExpr :: Maybe (Either String String)
            resultExpr = (\e -> valueToString reqResExprMsg =<<
                                  listToEither reqResExprMsg (executeJSONPath e value)
                         )
                     <$> jfiRequirementResultExpr jsonFormatInternal

        reqResExpr  <- except $ maybeEither resultExpr
        reqResExpr' <- ExceptT $ case reqResExpr of
                                   Nothing -> return $ Right Nothing
                                   Just x  -> fmap Just <$> parseExpr x

        return $ Requirement
                   { requirementName        = reqId
                   , requirementExpr        = reqExpr'
                   , requirementDescription = reqDesc
                   , requirementResultType  = reqResType
                   , requirementResultExpr  = reqResExpr'
                   }

  requirementDefs <- mapM requirementDef requirementValues

  return $ Spec internalVariableDefs externalVariableDefs requirementDefs

valueToString :: String -> Value -> Either String String
valueToString _   (String x) = Right $ unpack x
valueToString msg _          = Left $
  "The JSON value provided for " ++ msg ++ " does not contain a string"

listToEither :: String -> [a] -> Either String a
listToEither _   [x] = Right x
listToEither msg []  = Left $ "Failed to find a value for " ++ msg
listToEither msg _   = Left $ "Unexpectedly found multiple values for " ++ msg

-- | Parse a JSONPath expression, returning its element components.
parseJSONPath :: T.Text -> Either String [JSONPathElement]
parseJSONPath = first errorBundlePretty . parse (jsonPath eof) ""

showErrors :: Show a => Either a b -> Either String b
showErrors (Left s)  = Left (show s)
showErrors (Right x) = Right x

showErrorsM :: Show a => Maybe (Either a b) -> Either String (Maybe b)
showErrorsM Nothing          = Right Nothing
showErrorsM (Just (Left s))  = Left (show s)
showErrorsM (Just (Right x)) = Right (Just x)

-- | Wrap an 'Either' value in an @ExceptT m@ monad.
except :: Monad m => Either e a -> ExceptT e m a
except = ExceptT . return

-- | Swap the order in a Maybe and an Either monad.
maybeEither :: Maybe (Either a b) -> Either a (Maybe b)
maybeEither Nothing  = Right Nothing
maybeEither (Just e) = fmap Just e
