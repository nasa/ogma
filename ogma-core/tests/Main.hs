-- | Test ogma-core
module Main where

import Data.Monoid                    ( mempty )
import Test.Framework                 ( Test, defaultMainWithOpts )
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit                     ( assertBool )

-- Internal imports
import Command.CStructs2Copilot          ( cstructs2Copilot )
import Command.FRETComponentSpec2Copilot ( FRETComponentSpec2CopilotOptions (..)
                                         , fretComponentSpec2Copilot
                                         )
import Command.FRETReqsDB2Copilot        ( FRETReqsDB2CopilotOptions (..)
                                         , fretReqsDB2Copilot
                                         )
import Command.Result                    ( isSuccess )

-- | Run all unit tests on ogma-core.
main :: IO ()
main =
  defaultMainWithOpts tests mempty

-- | All unit tests for ogma-core.
tests :: [Test.Framework.Test]
tests =
  [
    testCase "fret-cmd-fret-cs-ok"
      (testFretComponentSpec2Copilot "tests/fret_good.json" True)
    -- Should pass

  , testCase "fret-cmd-fret-file-not-found"
      (testFretComponentSpec2Copilot "tests/file-invalid.json" False)
    -- Should fail because the file does not exist

  , testCase "fret-cmd-fret-parse-fail-1"
      (testFretComponentSpec2Copilot
         "tests/commands-fret-error-parsing-failed-1.json"
         False
      )
    -- Should fail because the opening bracket is [ and not {

  , testCase "fret-cmd-fret-parse-fail-2"
      (testFretComponentSpec2Copilot
         "tests/commands-fret-error-parsing-failed-2.json"
         False
      )
    -- Should fail because a field is missing in an external variable

  , testCase "fret-cmd-fret-parse-fail-3"
      (testFretComponentSpec2Copilot
         "tests/commands-fret-error-parsing-failed-3.json"
         False
      )
    -- Should fail because a field is missing in an internal variable

  , testCase "fret-reqs-db-cocospec"
      (testFretReqsDBCoCoSpec2Copilot "tests/fret-example1.json" True)
    -- Should pass

  , testCase "structs-parse-ok"
      (testCStructs2Copilot "tests/reduced_geofence_msgs.h" True)
    -- Should pass

  , testCase "structs-parse-fail-1"
      (testCStructs2Copilot "tests/reduced_geofence_msgs_bad.h" False)
    -- Should fail because a keyword is incorrect
  ]

-- | Test C struct parser and conversion to Copilot structs
-- for a particular file.
--
-- This test uses the Copilot backend for C header files, so it generates
-- Copilot types and instances.
--
-- This IO action fails if any of the following are true:
--   * The given file is not found or accessible.
--   * The format in the given file is incorrect.
--   * Ogma fails due to an internal error or bug.
--
testCStructs2Copilot :: FilePath  -- ^ Path to a C header file with structs
                     -> Bool
                     -> IO ()
testCStructs2Copilot file success = do
    result <- cstructs2Copilot file

    -- True if success is expected and detected, or niether expected nor
    -- detected.
    let testPass = success == isSuccess result

    assertBool errorMsg testPass
  where
    errorMsg = "The result of the transformation of the C header file "
               ++ file ++ " to Copilot struct declarations was unexpected."

-- | Test FRET Component Spec 2 Copilot transformation.
--
-- This test uses the Copilot backend for FRET files, so it generates a Copilot
-- file.
--
-- This IO action fails if any of the following are true:
--   * The given file is not found or accessible.
--   * The format in the given file is incorrect.
--   * Ogma fails due to an internal error or bug.
testFretComponentSpec2Copilot :: FilePath  -- ^ Path to a FRET/JSON requirements file
                              -> Bool
                              -> IO ()
testFretComponentSpec2Copilot file success = do
    let opts = FRETComponentSpec2CopilotOptions
                 { fretCS2CopilotUseCoCoSpec = False
                 , fretCS2CopilotIntType     = "Int64"
                 , fretCS2CopilotRealType    = "Float"
                 , fretCS2CopilotFilename    = "fret"
                 }
    result <- fretComponentSpec2Copilot file opts

    -- True if success is expected and detected, or niether expected nor
    -- detected.
    let testPass = success == isSuccess result

    assertBool errorMsg testPass
  where
    errorMsg = "The result of the transformation of FRET CS file "
               ++ file ++ " to Copilot was unexpected."

-- | Test FRET Component Spec 2 Copilot transformation.
--
-- This test uses the Copilot backend for FRET files with the CoCoSpec
-- frontend.
--
-- This IO action fails if any of the following are true:
--   * The given file is not found or accessible.
--   * The format in the given file is incorrect.
--   * Ogma fails due to an internal error or bug.
--
testFretReqsDBCoCoSpec2Copilot :: FilePath  -- ^ Path to a FRET/JSON
                                            --   requirements file
                               -> Bool
                               -> IO ()
testFretReqsDBCoCoSpec2Copilot file success = do
    let opts = FRETReqsDB2CopilotOptions
                 { fretReqsDB2CopilotUseCoCoSpec = True
                 , fretReqsDB2CopilotFilename    = "fret"
                 }
    result <- fretReqsDB2Copilot file opts

    -- True if success is expected and detected, or niether expected nor
    -- detected.
    let testPass = success == isSuccess result

    assertBool errorMsg testPass
  where
    errorMsg = "The result of the transformation of FRET CS file "
               ++ file ++ " to Copilot was unexpected."
