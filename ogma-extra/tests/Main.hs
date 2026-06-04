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
{-# LANGUAGE OverloadedStrings #-}
-- | Test ogma-extra internals.
module Main where

-- External imports
import           Control.Exception                    ( try )
import           Data.Aeson                           ( object, (.=) )
import qualified Data.ByteString                      as BS
import           Data.Either                          ( isLeft, isRight )
import           System.Directory                     ( createDirectoryIfMissing,
                                                        getTemporaryDirectory,
                                                        removePathForcibly )
import           System.FilePath                      ( (</>) )
import           Test.Framework                       ( Test,
                                                        defaultMainWithOpts )
import           Test.Framework.Providers.HUnit       ( testCase )
import           Test.Framework.Providers.QuickCheck2 ( testProperty )
import           Test.HUnit                           ( assertBool )
import           Test.QuickCheck                      ( Fun (Fun), Property,
                                                        (==>) )

-- Internal imports
import Data.List.Extra        ( headEither, toHead, toTail )
import System.Directory.Extra ( CopyTemplateError (..), copyTemplate )

-- | Run all unit tests on Ogma's core.
main :: IO ()
main =
  defaultMainWithOpts tests mempty

-- | All unit tests for Ogma's core.
tests :: [Test.Framework.Test]
tests =
  [ testProperty "Data.List.Extra.toHead (diag commutativity)" propToHead
  , testProperty "Data.List.Extra.toHead (identity)"           propToHeadId
  , testProperty "Data.List.Extra.toHead (composition)"        propToHeadCmp
  , testProperty "Data.List.Extra.toHead (length)"             propToHeadSameLength
  , testProperty "Data.List.Extra.toTail (diag commutativity)" propToTail
  , testProperty "Data.List.Extra.toTail (identity)"           propToTailId
  , testProperty "Data.List.Extra.toTail (composition)"        propToTailCmp
  , testProperty "Data.List.Extra.toTail (length)"             propToTailSameLength
  , testProperty "Data.List.Extra.headEither (Right case)"     propHeadEitherRight
  , testProperty "Data.List.Extra.headEither (Left case)"      propHeadEitherLeft
  , testCase "System.Directory.Extra.copyTemplate (valid template)"
      testCopyTemplateOk
  , testCase "System.Directory.Extra.copyTemplate (missing template dir)"
      testCopyTemplateMissingDir
  , testCase "System.Directory.Extra.copyTemplate (invalid encoding)"
      testCopyTemplateInvalidEncoding
  , testCase "System.Directory.Extra.copyTemplate (invalid syntax)"
      testCopyTemplateInvalidSyntax
  ]

-- * Data.List.Extra

-- | Test that 'toHead' applies a transformation to the head of a list.
propToHead :: [Int] -> Fun Int Int -> Property
propToHead xs (Fun _ f) = not (null xs) ==> f (head xs) == head (toHead f xs)

-- | Test that 'toHead' preserves the identity.
propToHeadId :: [Int] -> Bool
propToHeadId xs = toHead id xs == xs

-- | Test that 'toHead' preserves function composition.
propToHeadCmp :: [Int] -> Fun Int Int -> Fun Int Int -> Bool
propToHeadCmp xs (Fun _ f) (Fun _ g) = toHead (f . g) xs == toHead f (toHead g xs)

-- | Test that 'toHead' does not alter the length of the list.
propToHeadSameLength :: [Int] -> Fun Int Int -> Bool
propToHeadSameLength xs (Fun _ f) = length xs == length (toHead f xs)

-- | Test that 'toTail' applies a transformation to the tail of a list.
propToTail :: [Int] -> Fun Int Int -> Property
propToTail xs (Fun _ f) = not (null xs) ==> fmap f (tail xs) == tail (toTail f xs)

-- | Test that 'toTail' preserves the identity.
propToTailId :: [Int] -> Bool
propToTailId xs = toTail id xs == xs

-- | Test that 'toHead' preserves function composition.
propToTailCmp :: [Int] -> Fun Int Int -> Fun Int Int -> Bool
propToTailCmp xs (Fun _ f) (Fun _ g) = toTail (f . g) xs == toTail f (toTail g xs)

-- | Test that 'toTail' does not alter the length of the list.
propToTailSameLength :: [Int] -> Fun Int Int -> Bool
propToTailSameLength xs (Fun _ f) = length xs == length (toTail f xs)

-- | Test that 'headEither' returns a 'Right' value if applied to a non-empty
-- list.
propHeadEitherRight :: [Int] -> Property
propHeadEitherRight xs = not (null xs) ==> isRight (headEither xs)

-- | Test that 'headEither' returns a 'Left' value if applied to an empty list.
propHeadEitherLeft :: [Int] -> Property
propHeadEitherLeft xs = null xs ==> isLeft (headEither xs)

-- * System.Directory.Extra

-- | Test that 'copyTemplate' expands a valid template, substituting the
-- variables provided in the file contents.
testCopyTemplateOk :: IO ()
testCopyTemplateOk = do
    (templateDir, targetDir) <- makeTestDirs "ok"

    writeFile (templateDir </> "greeting.txt") "Hello, {{name}}!"

    result <- try $ copyTemplate templateDir subst targetDir

    contents <- readFile (targetDir </> "greeting.txt")

    let testPass = isRight (result :: Either CopyTemplateError ())
                     && contents == "Hello, World!"

    assertBool "Expansion of a valid template failed" testPass
  where
    subst = object [ "name" .= ("World" :: String) ]

-- | Test that 'copyTemplate' reports a list error when the template
-- directory does not exist.
testCopyTemplateMissingDir :: IO ()
testCopyTemplateMissingDir = do
    (templateDir, targetDir) <- makeTestDirs "missing"

    removePathForcibly templateDir

    result <- try $ copyTemplate templateDir (object []) targetDir

    let testPass = case result of
                     Left (CopyTemplateListError _ _) -> True
                     _                                -> False

    assertBool "Missing template directory not reported as a list error"
               testPass

-- | Test that 'copyTemplate' reports a decode error when a file in the
-- template is not a valid UTF-8 text file.
testCopyTemplateInvalidEncoding :: IO ()
testCopyTemplateInvalidEncoding = do
    (templateDir, targetDir) <- makeTestDirs "encoding"

    -- 0xC3 followed by 0x28 is an invalid UTF-8 byte sequence.
    BS.writeFile (templateDir </> "bad.txt") (BS.pack [0x68, 0xC3, 0x28])

    result <- try $ copyTemplate templateDir (object []) targetDir

    let testPass = case result of
                     Left (CopyTemplateDecodeError _ _) -> True
                     _                                  -> False

    assertBool "Invalid UTF-8 file not reported as a decode error" testPass

-- | Test that 'copyTemplate' reports a parse error when a file in the
-- template contains invalid template syntax.
testCopyTemplateInvalidSyntax :: IO ()
testCopyTemplateInvalidSyntax = do
    (templateDir, targetDir) <- makeTestDirs "syntax"

    writeFile (templateDir </> "bad.txt") "{{#unclosed\nsome text\n"

    result <- try $ copyTemplate templateDir (object []) targetDir

    let testPass = case result of
                     Left (CopyTemplateParseError _ _) -> True
                     _                                 -> False

    assertBool "Invalid template syntax not reported as a parse error"
               testPass

-- | Create empty template and target directories for a test with the given
-- name, removing any leftovers from prior runs.
makeTestDirs :: String -> IO (FilePath, FilePath)
makeTestDirs name = do
  tmpDir <- getTemporaryDirectory
  let templateDir = tmpDir </> ("ogma-extra-tests-template-" ++ name)
      targetDir   = tmpDir </> ("ogma-extra-tests-target-" ++ name)
  removePathForcibly templateDir
  removePathForcibly targetDir
  createDirectoryIfMissing True templateDir
  return (templateDir, targetDir)
