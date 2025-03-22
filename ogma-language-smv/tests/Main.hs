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
