-- {-# LANGUAGE ScopedTypeVariables #-}
-- Copyright 2024 United States Government as represented by the Administrator
-- of the National Aeronautics and Space Administration. All Rights Reserved.
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
