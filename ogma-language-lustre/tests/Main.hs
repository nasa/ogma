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
-- | Test Lustre language library.
module Main where

-- External imports
import Data.Either                          ( isLeft, isRight )
import Test.Framework                       ( Test, defaultMainWithOpts )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck                      ( Property )
import Test.QuickCheck.Monadic              ( assert, monadicIO, run )

-- Internal imports
import qualified Language.Lustre.ParLustre as Lustre ( myLexer, pBoolSpec )

-- | Run all unit tests for the Lustre parser.
main :: IO ()
main =
  defaultMainWithOpts tests mempty

-- | All unit tests for the Lustre parser.
tests :: [Test.Framework.Test]
tests =
  [ testProperty "Parse Lustre (correct case)"   propParseLustreOk
  , testProperty "Parse Lustre (incorrect case)" propParseLustreFail
  ]

-- | Test the Lustre parser on a well-formed boolean specification.
propParseLustreOk :: Property
propParseLustreOk = monadicIO $ do
  content <- run $ readFile "tests/lustre_good"
  let program = Lustre.pBoolSpec $ Lustre.myLexer content
  assert (isRight program)

-- | Test the Lustre parser on an incorrect boolean specification.
propParseLustreFail :: Property
propParseLustreFail = monadicIO $ do
  content <- run $ readFile "tests/lustre_bad"
  let program = Lustre.pBoolSpec $ Lustre.myLexer content
  assert (isLeft program)
