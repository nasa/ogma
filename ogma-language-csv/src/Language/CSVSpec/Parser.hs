-- {-# LANGUAGE ScopedTypeVariables #-}
-- Copyright 2024 United States Government as represented by the Administrator
-- of the National Aeronautics and Space Administration. All Rights Reserved.
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

-- | Parser for Ogma specs stored in CSV files.
module Language.CSVSpec.Parser where

-- External imports
import           Control.Monad           (forM, sequence)
import           Data.Csv                (HasHeader (NoHeader), Record, decode)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vector             as V

-- External imports: ogma-spec
import Data.OgmaSpec (Requirement (..), Spec (Spec))

-- | Area of the CSV file that contains the information of interest.
data CSVFormat = CSVFormat
    { skipHeaders               :: Bool
    , specRequirementId         :: Int
    , specRequirementDesc       :: Maybe Int
    , specRequirementExpr       :: Int
    , specRequirementResultType :: Maybe Int
    , specRequirementResultExpr :: Maybe Int
    }
  deriving (Show, Read)

-- | Parse a CSV file and extract a Spec from it.
--
-- An auxiliary function must be provided to parse the requirement expressions.
--
-- Fails if any of the columns indicate a column out of range, of if the CSV is
-- malformed.
parseCSVSpec :: (String -> IO (Either String a)) -- ^ Parser for expressions.
             -> a                                -- ^ Default property value.
             -> CSVFormat                        -- ^ CSV file format spec.
             -> String                           -- ^ String containing CSV.
             -> IO (Either String (Spec a))
parseCSVSpec parseExpr _defA csvFormat value = do
  let bsToString = T.unpack . T.decodeUtf8
      stringToBS = TL.encodeUtf8 . TL.pack

  let internalVariableDefs = []
      externalVariableDefs = []

      csvData = stringToBS value

  case decode NoHeader csvData of
    Left err -> return $ Left err
    Right v  -> do
      let vl = V.toList (v :: V.Vector Record)
          v' = if skipHeaders csvFormat then tail vl else vl
      rs <- forM v' $ \row -> do
        let rowL = V.toList row
        expr  <- parseExpr $ bsToString $
                  rowL !! specRequirementExpr csvFormat
        exprR <- maybe (return $ Right Nothing)
                       (\ix -> fmap Just <$>
                                 (parseExpr $ bsToString $ rowL !! ix))
                       (specRequirementResultExpr csvFormat)
        case (expr, exprR) of
          (Left e, _)
            -> return $ Left $ "The CSV data could not be parsed: " ++ e

          (_, Left e)
            -> return $ Left $ "The CSV data could not be parsed: " ++ e

          (Right e, Right rE) -> return $ Right $
            Requirement
              { requirementName =
                  bsToString $ rowL !! specRequirementId csvFormat
              , requirementDescription =
                  maybe "" (bsToString . (rowL !!)) $
                    specRequirementDesc csvFormat
              , requirementExpr = e
              , requirementResultType =
                  fmap (bsToString . (rowL !!)) $
                    specRequirementResultType csvFormat
              , requirementResultExpr = rE
              }

      case sequence rs of
        Left err  -> return $ Left err
        Right rs' -> return $ Right $
                       Spec internalVariableDefs externalVariableDefs rs'
