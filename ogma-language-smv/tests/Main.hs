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
-- | Test SMV language library.
module Main where

-- External imports
import Data.Either                          ( isLeft, isRight )
import Test.Framework                       ( Test, defaultMainWithOpts )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck                      ( Property )
import Test.QuickCheck.Monadic              ( assert, monadicIO, run )

-- Internal imports
import qualified Language.SMV.ParSMV as SMV ( myLexer, pBoolSpec )

-- | Run all unit tests for the SMV parser.
main :: IO ()
main =
  defaultMainWithOpts tests mempty

-- | All unit tests for the SMV parser.
tests :: [Test.Framework.Test]
tests =
  [ testProperty "Parse SMV (correct case)"   propParseSMVOk
  , testProperty "Parse SMV (incorrect case)" propParseSMVFail
  ]

-- | Test the SMV parser on a well-formed boolean specification.
propParseSMVOk :: Property
propParseSMVOk = monadicIO $ do
  content <- run $ readFile "tests/smv_good"
  let program = SMV.pBoolSpec $ SMV.myLexer content
  assert (isRight program)

-- | Test the SMV parser on an incorrect boolean specification.
propParseSMVFail :: Property
propParseSMVFail = monadicIO $ do
  content <- run $ readFile "tests/smv_bad"
  let program = SMV.pBoolSpec $ SMV.myLexer content
  assert (isLeft program)
