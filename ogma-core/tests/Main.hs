-- | Test ogma-core
module Main where

import Data.Monoid                    ( mempty )
import Test.Framework                 ( Test, defaultMainWithOpts )
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit                     ( assertBool )
import System.Directory               ( getTemporaryDirectory )

-- Internal imports
import Command.CStructs2Copilot (cstructs2Copilot)
import Command.Result           (isSuccess)
import Command.Standalone       (CommandOptions (..), command)

-- | Run all unit tests on ogma-core.
main :: IO ()
main =
  defaultMainWithOpts tests mempty

-- | All unit tests for ogma-core.
tests :: [Test.Framework.Test]
tests =
  [
    testCase "standalone-cmd-fcs-ok"
      (testStandaloneFCS "tests/fcs_good.json" True)
    -- Should pass

  , testCase "standalone-cmd-fsc-file-not-found"
      (testStandaloneFCS "tests/file-invalid.json" False)
    -- Should fail because the file does not exist

  , testCase "standalone-cmd-fcs-parse-fail-1"
      (testStandaloneFCS
         "tests/commands-fcs-error-parsing-failed-1.json"
         False
      )
    -- Should fail because the opening bracket is [ and not {

  , testCase "standalone-cmd-fcs-parse-fail-2"
      (testStandaloneFCS
         "tests/commands-fcs-error-parsing-failed-2.json"
         False
      )
    -- Should fail because a field is missing in an external variable

  , testCase "standalone-cmd-fcs-parse-fail-3"
      (testStandaloneFCS
         "tests/commands-fcs-error-parsing-failed-3.json"
         False
      )
    -- Should fail because a field is missing in an internal variable

  , testCase "standalone-reqs-db-lustre"
      (testStandaloneFDB "tests/fdb-example1.json" True)
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

-- | Test standalone backend.
--
-- This test uses the standalone, so it generates a Copilot file.
--
-- This IO action fails if any of the following are true:
--   * The given file is not found or accessible.
--   * The format in the given file is incorrect.
--   * Ogma fails due to an internal error or bug.
testStandaloneFCS :: FilePath  -- ^ Path to a input file
                  -> Bool
                  -> IO ()
testStandaloneFCS file success = do
    targetDir <- getTemporaryDirectory
    let opts = CommandOptions
                 { commandInputFile   = file
                 , commandFormat      = "fcs"
                 , commandPropFormat  = "smv"
                 , commandTypeMapping = [("int", "Int64"), ("real", "Float")]
                 , commandFilename    = "monitor"
                 , commandTargetDir   = targetDir
                 , commandTemplateDir = Nothing
                 , commandPropVia     = Nothing
                 , commandExtraVars   = Nothing
                 }
    result <- command opts

    -- True if success is expected and detected, or niether expected nor
    -- detected.
    let testPass = success == isSuccess result

    assertBool errorMsg testPass
  where
    errorMsg = "The result of the transformation of input file "
               ++ file ++ " to Copilot was unexpected."

-- | Test standalone backend with FDB format.
--
-- This test uses the standalone backend with the FDB format and the Lustre
-- property format.
--
-- This IO action fails if any of the following are true:
--   * The given file is not found or accessible.
--   * The format in the given file is incorrect.
--   * Ogma fails due to an internal error or bug.
--
testStandaloneFDB :: FilePath  -- ^ Path to input file
                  -> Bool
                  -> IO ()
testStandaloneFDB file success = do
    targetDir <- getTemporaryDirectory
    let opts = CommandOptions
                 { commandInputFile   = file
                 , commandFormat      = "fdb"
                 , commandPropFormat  = "lustre"
                 , commandTypeMapping = []
                 , commandFilename    = "monitor"
                 , commandTargetDir   = targetDir
                 , commandTemplateDir = Nothing
                 , commandPropVia     = Nothing
                 , commandExtraVars   = Nothing
                 }
    result <- command opts

    -- True if success is expected and detected, or niether expected nor
    -- detected.
    let testPass = success == isSuccess result

    assertBool errorMsg testPass
  where
    errorMsg = "The result of the transformation of input file "
               ++ file ++ " to Copilot was unexpected."
