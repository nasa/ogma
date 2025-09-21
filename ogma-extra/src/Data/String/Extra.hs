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
-- | Auxiliary functions for working with values of type 'String'.
module Data.String.Extra
    (
      -- * Safe I/O
      safeReadFile

      -- * String transformation
    , pascalCase

      -- * String sanitization
    , sanitizeLCIdentifier
    , sanitizeUCIdentifier
    )
  where

-- External imports
import Control.Exception ( catch )
import Data.Char         ( toLower, toUpper )
import System.IO.Error   ( isDoesNotExistError )

-- * Safe I/O

-- | Safely read a file into a 'String', returning a 'Left' error message if
-- the file cannot be opened.

-- This function could also be placed in System.IO. However, we decide to
-- include it in this module for symmetry with an analogous function for
-- ByteString, included in Data.ByteString.Extra.
safeReadFile :: FilePath -> IO (Either String String)
safeReadFile fp =
  catch (return <$> readFile fp) $ \e ->
    if isDoesNotExistError e
      then return $ Left $ strStringFileNotFound fp
      else return $ Left $ strStringCannotOpenFile fp

-- ** Error messages

-- | File-not-found message.
strStringFileNotFound :: FilePath -> String
strStringFileNotFound fp = "File not found: " ++ fp

-- | Cannot-open-file message.
strStringCannotOpenFile :: FilePath -> String
strStringCannotOpenFile fp = "Error opening file: " ++ fp

-- * Transformation

-- | Convert a string with underscores to PascalCase.
pascalCase :: String -> String
pascalCase = concatMap capitalize . parts
  where
    capitalize (x:xs) = toUpper x : map toLower xs
    capitalize [] = []

    parts :: String -> [String]
    parts "" = []
    parts s =
      let (l, r) = break (== '_') s
      in l : case r of
               []     -> []
               (_:rs) -> parts rs

-- * Sanitization

-- | Remove extraneous characters from an identifier and make the starting
-- character lowercase.
--
-- This function currently replaces hyphens with underscores.
sanitizeLCIdentifier :: String -> String
sanitizeLCIdentifier = headToLower . map sanitizeCharacter
  where
    sanitizeCharacter '-' = '_'
    sanitizeCharacter x   = x

    headToLower :: [Char] -> [Char]
    headToLower []     = []
    headToLower (x:xs) = toLower x : xs

-- | Remove extraneous characters from an identifier and make the starting
-- character uppercase.
--
-- This function currently replaces hyphens with underscores.
sanitizeUCIdentifier :: String -> String
sanitizeUCIdentifier = headToUpper . map sanitizeCharacter
  where
    sanitizeCharacter '-' = '_'
    sanitizeCharacter x   = x

    headToUpper :: [Char] -> [Char]
    headToUpper []     = []
    headToUpper (x:xs) = toUpper x : xs
