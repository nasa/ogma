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
-- | Test FRETReqsDB language library.
module Main where

-- External imports
import Data.Aeson                           ( eitherDecode )
import Data.Either                          ( isLeft, isRight )
import Test.Framework                       ( Test, defaultMainWithOpts )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck                      ( Property )
import Test.QuickCheck.Monadic              ( assert, monadicIO, run )

-- External imports: auxiliary
import Data.ByteString.Extra as B ( safeReadFile )

-- Internal imports
import Language.FRETReqsDB.AST ( FRETReqsDB )

-- | Run all unit tests for the FRETReqsDB parser.
main :: IO ()
main =
  defaultMainWithOpts tests mempty

-- | All unit tests for the FRETReqsDB parser.
tests :: [Test.Framework.Test]
tests =
  [ testProperty "Parse FRETReqsDB (correct case)"   propParseFRETReqsDBOk
  -- , testProperty "Parse FRETReqsDB (incorrect case)" propParseFRETReqsDBFail
  ]

-- | Test the FRETReqsDB parser on a well-formed boolean specification.
propParseFRETReqsDBOk :: Property
propParseFRETReqsDBOk = monadicIO $ do
  content <- run $ parseFretReqsDB "tests/fret_good.json"
  assert (isRight content)

-- | Test the FRETReqsDB parser on an incorrect boolean specification.
propParseFRETReqsDBFail :: Property
propParseFRETReqsDBFail = monadicIO $ do
  componentSpec <- run $ parseFretReqsDB "tests/fret_bad.json"
  assert (isLeft componentSpec)

-- | Parse a JSON file containing a FRET Requirements Database.
--
-- Returns a 'Left' with an error message if the file does not have the correct
-- format.
--
-- Throws an exception if the file cannot be read.
parseFretReqsDB :: FilePath -> IO (Either String FRETReqsDB)
parseFretReqsDB fp = do
  reqsDB <- B.safeReadFile fp
  let e = eitherDecode =<< reqsDB
  print e
  return e
