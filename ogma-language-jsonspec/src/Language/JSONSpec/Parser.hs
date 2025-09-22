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
import           Data.Aeson            (FromJSON (..), Value (..), decode, (.:))
import           Data.Aeson.Key        (toString)
import qualified Data.Aeson.KeyMap     as M
import           Data.Aeson.Types      (prependFailure, typeMismatch)
import           Data.Bifunctor        (first)
import           Data.ByteString.Lazy  (fromStrict)
import           Data.JSONPath.Execute
import           Data.JSONPath.Parser
import           Data.JSONPath.Types
import           Data.Text             (pack, unpack)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.Text.IO          as T
import           Text.Megaparsec       (eof, errorBundlePretty, parse)

-- External imports: ogma-spec
import Data.OgmaSpec

data JSONFormat = JSONFormat
    { specInternalVars          :: Maybe String
    , specInternalVarId         :: String
    , specInternalVarExpr       :: String
    , specInternalVarType       :: Maybe String
    , specExternalVars          :: Maybe String
    , specExternalVarId         :: String
    , specExternalVarType       :: Maybe String
    , specRequirements          :: String
    , specRequirementId         :: String
    , specRequirementDesc       :: Maybe String
    , specRequirementExpr       :: String
    , specRequirementResultType :: Maybe String
    , specRequirementResultExpr :: Maybe String
    }
  deriving (Read)

data JSONFormatInternal = JSONFormatInternal
  { jfiInternalVars          :: Maybe [JSONPathElement]
  , jfiInternalVarId         :: [JSONPathElement]
  , jfiInternalVarExpr       :: [JSONPathElement]
  , jfiInternalVarType       :: Maybe [JSONPathElement]
  , jfiExternalVars          :: Maybe [JSONPathElement]
  , jfiExternalVarId         :: [JSONPathElement]
  , jfiExternalVarType       :: Maybe [JSONPathElement]
  , jfiRequirements          :: [JSONPathElement]
  , jfiRequirementId         :: [JSONPathElement]
  , jfiRequirementDesc       :: Maybe [JSONPathElement]
  , jfiRequirementExpr       :: [JSONPathElement]
  , jfiRequirementResultType :: Maybe [JSONPathElement]
  , jfiRequirementResultExpr :: Maybe [JSONPathElement]
  }

parseJSONFormat :: JSONFormat -> Either String JSONFormatInternal
parseJSONFormat jsonFormat = do
  jfi2  <- showErrorsM $ fmap (parseJSONPath . pack) $ specInternalVars    jsonFormat
  jfi3  <- showErrors $ parseJSONPath $ pack $ specInternalVarId   jsonFormat
  jfi4  <- showErrors $ parseJSONPath $ pack $ specInternalVarExpr jsonFormat
  jfi5  <- showErrorsM $ fmap (parseJSONPath . pack) $ specInternalVarType jsonFormat
  jfi6  <- showErrorsM $ fmap (parseJSONPath . pack) $ specExternalVars    jsonFormat
  jfi7  <- showErrors $ parseJSONPath $ pack $ specExternalVarId   jsonFormat
  jfi8  <- showErrorsM $ fmap (parseJSONPath . pack) $ specExternalVarType jsonFormat
  jfi9  <- showErrors $ parseJSONPath $ pack $ specRequirements    jsonFormat
  jfi10 <- showErrors $ parseJSONPath $ pack $ specRequirementId   jsonFormat
  jfi11 <- showErrorsM $ fmap (parseJSONPath . pack) $ specRequirementDesc jsonFormat
  jfi12 <- showErrors $ parseJSONPath $ pack $ specRequirementExpr jsonFormat
  jfi13 <- showErrorsM $ fmap (parseJSONPath . pack) $ specRequirementResultType jsonFormat
  jfi14 <- showErrorsM $ fmap (parseJSONPath . pack) $ specRequirementResultExpr jsonFormat
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

parseJSONSpec :: (String -> IO (Either String a)) -> JSONFormat -> Value -> IO (Either String (Spec a))
parseJSONSpec parseExpr jsonFormat value = runExceptT $ do
  jsonFormatInternal <- except $ parseJSONFormat jsonFormat

  let values :: [Value]
      values = maybe [] (`executeJSONPath` value) (jfiInternalVars jsonFormatInternal)

      internalVarDef :: Value -> Either String InternalVariableDef
      internalVarDef value = do
        let msg = "internal variable name"
        varId   <- valueToString msg =<< (listToEither msg (executeJSONPath (jfiInternalVarId jsonFormatInternal) value))

        let msg = "internal variable type"
        varType <- maybe (Right "") (\e -> valueToString msg =<< (listToEither msg (executeJSONPath e value))) (jfiInternalVarType jsonFormatInternal)

        let msg = "internal variable expr"
        varExpr <- valueToString msg =<< (listToEither msg (executeJSONPath (jfiInternalVarExpr jsonFormatInternal) value))

        return $ InternalVariableDef
                   { internalVariableName    = varId
                   , internalVariableType    = varType
                   , internalVariableExpr    = varExpr
                   }

  internalVariableDefs <- except $ mapM internalVarDef values

  let values :: [Value]
      values = maybe [] (`executeJSONPath` value) (jfiExternalVars jsonFormatInternal)

      externalVarDef :: Value -> Either String ExternalVariableDef
      externalVarDef value = do

        let msg = "external variable name"
        varId   <- valueToString msg =<< (listToEither msg (executeJSONPath (jfiExternalVarId jsonFormatInternal) value))

        let msg = "external variable type"
        varType <- maybe (Right "") (\e -> valueToString msg =<< (listToEither msg (executeJSONPath e value))) (jfiExternalVarType jsonFormatInternal)

        return $ ExternalVariableDef
                   { externalVariableName    = varId
                   , externalVariableType    = varType
                   }

  externalVariableDefs <- except $ mapM externalVarDef values

  let values :: [Value]
      values = executeJSONPath (jfiRequirements jsonFormatInternal) value

      -- requirementDef :: Value -> Either String (Requirement a)
      requirementDef value = do
        let msg = "Requirement name"
        reqId <- except $ valueToString msg =<< (listToEither msg (executeJSONPath (jfiRequirementId jsonFormatInternal) value))

        let msg = "Requirement expression"
        reqExpr <- except $ valueToString msg =<< (listToEither msg (executeJSONPath (jfiRequirementExpr jsonFormatInternal) value))
        reqExpr' <- ExceptT $ parseExpr reqExpr

        let msg = "Requirement description"
        reqDesc <- except $ maybe (Right "") (\e -> valueToString msg =<< (listToEither msg (executeJSONPath e value))) (jfiRequirementDesc jsonFormatInternal)

        let msg = "Requirement result type"
            ty :: Maybe (Either String String)
            ty = (\e -> valueToString msg =<< (listToEither msg (executeJSONPath e value))) <$> (jfiRequirementResultType jsonFormatInternal)
        reqResType <- except $ maybeEither ty

        let msg = "Requirement result expression"
            resultExpr :: Maybe (Either String String)
            resultExpr = (\e -> valueToString msg =<< (listToEither msg (executeJSONPath e value))) <$> (jfiRequirementResultExpr jsonFormatInternal)

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

  requirements <- mapM requirementDef values

  return $ Spec internalVariableDefs externalVariableDefs requirements

valueToString :: String -> Value -> Either String String
valueToString msg (String x) = Right $ unpack x
valueToString msg _          = Left $ "The JSON value provided for " ++ msg ++ " does not contain a string"

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
