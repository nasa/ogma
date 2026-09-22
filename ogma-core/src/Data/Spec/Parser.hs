{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Copyright 2022 United States Government as represented by the Administrator
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
-- | Parsing of specs.
module Data.Spec.Parser
    ( readInputExpr
    , readInputFile
    )
  where

-- External imports
import qualified Control.Exception    as E
import           Control.Monad.Except (ExceptT (..))
import           Data.Aeson           (eitherDecode)
import qualified Data.ByteString.Lazy as L
import           Data.List            (isInfixOf, isPrefixOf)
import           System.Directory     (doesFileExist)
import           System.FilePath      ((</>))
import           System.Process       (readProcess)

-- External imports: auxiliary
import Data.ByteString.Extra as B (safeReadFile)

-- External imports: ogma
import Data.OgmaSpec            (Requirement (..), Spec (..))
import Language.CSVSpec.Parser  (parseCSVSpec)
import Language.JSONSpec.Parser (parseJSONSpec)
import Language.XLSXSpec.Parser (parseXLSXSpec)
import Language.XMLSpec.Parser  (parseXMLSpec)
import Language.YAMLSpec.Parser (parseYAMLSpec)

-- Internal imports: auxiliary
import Command.Errors    (ErrorTriplet(..), ErrorCode)
import Data.Either.Extra (mapLeft)
import Data.ExprPair     (ExprPairT(..))
import Data.Location     (Location (..))
import Paths_ogma_core   (getDataDir)

-- | Process input specification from a single expression and return its
-- abstract representation.
readInputExpr :: String
              -> String
              -> Maybe String
              -> ExprPairT a
              -> ExceptT ErrorTriplet IO (Spec a)
readInputExpr expr propFormatName propVia exprT =
  ExceptT $ do
    let ExprPairT parse replace print ids def = exprT

    let wrapper = wrapVia propVia parse

    result <- wrapper expr

    let spec = do
          expr' <- result
          let req = Requirement "triggerCondition" expr' "" Nothing Nothing
          return $ Spec [] [] [ req ]

    -- Return the spec, transforming the error message if applicable.
    pure $ mapLeft (cannotReadConditionExpr expr) spec

-- | Process input specification, if available, and return its abstract
-- representation.
readInputFile :: FilePath
              -> String
              -> String
              -> Maybe String
              -> ExprPairT a
              -> ExceptT ErrorTriplet IO (Spec a)
readInputFile fp formatName propFormatName propVia exprT =
  ExceptT $ do
    let ExprPairT parse replace print ids def = exprT

    let wrapper = wrapVia propVia parse
    -- Obtain format file.
    --
    -- A format name that exists as a file in the disk always takes preference
    -- over a file format included with Ogma. A file format with a forward
    -- slash in the name is always assumed to be a user-provided filename.
    -- Regardless of whether the file is user-provided or known to Ogma, we
    -- check (again) whether the file exists, and print an error message if
    -- not.
    exists  <- doesFileExist formatName
    dataDir <- getDataDir
    let formatFile
          | isInfixOf "/" formatName || exists
          = formatName
          | otherwise
          = dataDir </> "data" </> "formats" </>
               (formatName ++ "_" ++ propFormatName)
    formatMissing <- not <$> doesFileExist formatFile

    if formatMissing
      then return $ Left $ commandIncorrectFormatSpec formatFile
      else do
        res <- do
          format <- readFile formatFile

          -- All of the following operations use Either to return error
          -- messages.  The use of the monadic bind to pass arguments from one
          -- function to the next will cause the program to stop at the
          -- earliest error.
          if | isPrefixOf "XMLFormat" format
             -> do let xmlFormat = read format
                   content <- readFile fp
                   parseXMLSpec wrapper def xmlFormat content
             | isPrefixOf "CSVFormat" format
             -> do let csvFormat = read format
                   content <- readFile fp
                   parseCSVSpec wrapper def csvFormat content
             | isPrefixOf "XLSXFormat" format
             -> do let xlsxFormat = read format
                   content <- L.readFile fp
                   parseXLSXSpec wrapper def xlsxFormat content
             | isPrefixOf "YAMLFormat" format
             -> do let yamlFormat = read format
                   content <- B.safeReadFile fp
                   case content of
                     Left e  -> return $ Left e
                     Right b ->
                       parseYAMLSpec wrapper yamlFormat fp (L.toStrict b)
             | otherwise
             -> do let jsonFormat = read format
                   content <- B.safeReadFile fp
                   case content of
                     Left e  -> return $ Left e
                     Right b -> do case eitherDecode b of
                                     Left e  -> return $ Left e
                                     Right v ->
                                       parseJSONSpec
                                         wrapper
                                         jsonFormat
                                         fp
                                         v
        case res of
          Left e  -> return $ Left $ cannotOpenInputFile fp
          Right x -> return $ Right x

-- | Exception handler to deal with the case in which the trigger expression
-- cannot be understood.
cannotReadConditionExpr :: String -> String -> ErrorTriplet
cannotReadConditionExpr expr errorMsg =
    ErrorTriplet ecCannotReadConditionExpr msg LocationNothing
  where
    msg =
      "cannot parse condition or trigger expression " ++ show expr ++ ":"
      ++ errorMsg

-- | Exception handler to deal with the case in which the input file cannot be
-- opened.
cannotOpenInputFile :: FilePath -> ErrorTriplet
cannotOpenInputFile file =
    ErrorTriplet ecCannotOpenInputFile msg (LocationFile file)
  where
    msg =
      "cannot open input specification file " ++ file

-- | Error message associated to the format file not being found.
commandIncorrectFormatSpec :: FilePath -> ErrorTriplet
commandIncorrectFormatSpec formatFile =
    ErrorTriplet ecIncorrectFormatFile msg (LocationFile formatFile)
  where
    msg =
      "The format specification " ++ formatFile ++ " does not exist or is not "
      ++ "readable"

-- ** Error codes

-- | Error: the trigger expression provided by the user cannot be parsed.
ecCannotReadConditionExpr :: ErrorCode
ecCannotReadConditionExpr = 1

-- | Error: the input specification provided by the user cannot be opened.
ecCannotOpenInputFile :: ErrorCode
ecCannotOpenInputFile = 1

-- | Error: the format file cannot be opened.
ecIncorrectFormatFile :: ErrorCode
ecIncorrectFormatFile = 1

-- | Parse a property using an auxiliary program to first translate it, if
-- available.
--
-- If a program is given, it is first called on the property, and then the
-- result is parsed with the parser passed as an argument. If a program is not
-- given, then the parser is applied to the given string.
wrapVia :: Maybe String                -- ^ Auxiliary program to translate the
                                       -- property.
        -> (String -> Either String a) -- ^ Parser used on the result.
        -> String                      -- ^ Property to parse.
        -> IO (Either String a)
wrapVia Nothing  parse s = return (parse s)
wrapVia (Just f) parse s =
  E.handle (\(e :: E.IOException) -> return $ Left $ show e) $ do
    out <- readProcess f [] s
    return $ parse out
