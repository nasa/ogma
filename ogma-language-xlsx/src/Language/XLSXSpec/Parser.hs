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

-- | Parser for Ogma specs stored in XLSX files.
module Language.XLSXSpec.Parser (XLSXFormat(..), parseXLSXSpec) where

-- External imports
import           Codec.Xlsx           (Cell, CellValue (..), ColumnIndex (..),
                                       ParseError (..), _cellValue, _wsCells,
                                       _xlSheets, toRows, toXlsxEither)
import           Control.Monad        (forM, sequence)
import qualified Data.ByteString.Lazy as L
import           Data.List            (lookup)
import           Data.Maybe           (catMaybes, fromJust, isNothing)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T

-- External imports: ogma-spec
import Data.OgmaSpec (Requirement (..), Spec (Spec))

-- | Area of the CSV file that contains the information of interest.
data XLSXFormat = XLSXFormat
    { skipHeaders               :: Bool
    , specRequirementSheet      :: String
    , specRequirementId         :: Int
    , specRequirementDesc       :: Maybe Int
    , specRequirementExpr       :: Int
    , specRequirementResultType :: Maybe Int
    , specRequirementResultExpr :: Maybe Int
    }
  deriving (Read)

-- | Parse a XLSX file and extract a Spec from it.
--
-- An auxiliary function must be provided to parse the requirement expressions.
--
-- Fails if the sheet does not exist, any of the columns indicate a column out
-- of range, if the XLSX is malformed.
parseXLSXSpec :: (String -> IO (Either String a)) -- ^ Parser for expressions.
              -> a                                -- ^ Default property value.
              -> XLSXFormat                       -- ^ Spec format.
              -> L.ByteString                     -- ^ String containing XLSX.
              -> IO (Either String (Spec a))
parseXLSXSpec parseExpr _defA xlsxFormat value = do

  -- The XLSX spec parser does not current support reading lists of internal or
  -- external variables from the XLSX file.
  let internalVariableDefs = []
      externalVariableDefs = []

  -- Obtain sheets and locate sheet needed.
  let sheets = _xlSheets <$> toXlsxEither value

  case sheets of
    Left err      -> return $ Left $ showParseError err
    Right sheets' -> do

      let sheet = fromJust
                $ lookup (T.pack (specRequirementSheet xlsxFormat)) sheets'

      -- Obtain rows, discarding the header row if needed.
      let rows  = if skipHeaders xlsxFormat then tail rows' else rows'
          rows' = toRows $ _wsCells sheet

      -- Turn each row into a requirement, skipping rows without the necessary
      -- information.
      rs <- forM rows $ \(_, row) -> do
        if emptyRow xlsxFormat row
          then return $ Right Nothing
          else do

            -- Read the two expressions in each row (the condition expression
            -- and the result expression), and return a requirement.
            expr <- parseExpr $ rowCell (specRequirementExpr xlsxFormat) row
            exprR <- maybe (return $ Right Nothing)
                           (\ix -> fmap Just <$> parseExpr (rowCell ix row))
                           (specRequirementResultExpr xlsxFormat)

            case (expr, exprR) of
              (Left e,  _) ->
                 return $ Left $ "The XLSX data could not be parsed: " ++ e

              (_, Left e) ->
                 return $ Left $ "The XLSX data could not be parsed: " ++ e

              (Right e, Right rE) -> return $ Right $ Just
                Requirement
                  { requirementName =
                      rowCell (specRequirementId xlsxFormat) row
                  , requirementDescription =
                      maybe "" (`rowCell` row) (specRequirementDesc xlsxFormat)
                  , requirementExpr = e
                  , requirementResultType =
                      fmap
                        (`rowCell` row)
                        (specRequirementResultType xlsxFormat)
                  , requirementResultExpr = rE
                  }

      case fmap catMaybes (sequence rs) of
        Left err  -> return $ Left err
        Right rs' -> return $ Right $
                       Spec internalVariableDefs externalVariableDefs rs'

-- * Auxiliary functions

-- | A row is empty if any of the cells needed is empty.
emptyRow :: XLSXFormat -> [(ColumnIndex, Cell)] -> Bool
emptyRow xlsxFormat row =
     emptyCell (specRequirementExpr xlsxFormat) row
  || emptyCell (specRequirementId xlsxFormat) row
  || maybe False (`emptyCell` row)
       (specRequirementDesc xlsxFormat)
  || maybe False (`emptyCell` row)
       (specRequirementResultExpr xlsxFormat)
  || maybe False (`emptyCell` row)
       (specRequirementResultType xlsxFormat)

-- | A cell is empty if the cell cannot be found in the row.
emptyCell :: Int -> [(ColumnIndex, Cell)] -> Bool
emptyCell i row = isNothing (lookup (ColumnIndex i) row)

-- | Obtain a cell from a row, as a 'String'.
--
-- PRE: The cell exists and has a value.
rowCell :: Int -> [(ColumnIndex, Cell)] -> String
rowCell i row = cellValueToString
              $ fromJust
              $ _cellValue
              $ fromJust
              $ lookup (ColumnIndex i) row

-- | Convert a cell value into a 'String'.
cellValueToString :: CellValue -> String
cellValueToString (CellText txt) = T.unpack txt
cellValueToString (CellDouble n) = show n
cellValueToString (CellBool b)   = show b
cellValueToString (CellRich _)   = "(unsupported)"
cellValueToString (CellError _)  = "(error)"

-- | Show a parse error message.
showParseError :: ParseError -> String
showParseError (InvalidZipArchive string) = "Invalid zip archive: " ++ string
showParseError (MissingFile fp)           = "Missing file: " ++ fp
showParseError (InvalidFile fp txt)       = "Invalid file: " ++ fp
showParseError (InvalidRef fp refId)      = "Invalid reference in file: " ++ fp
showParseError (InconsistentXlsx txt)     = "Inconsistent XLSX file"
