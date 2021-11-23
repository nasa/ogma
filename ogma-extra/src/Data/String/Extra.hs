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
-- | Auxiliary functions for working with values of type 'String'.
module Data.String.Extra
    (
      -- * Safe I/O
      safeReadFile

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
