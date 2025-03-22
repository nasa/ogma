-- | Test Ogma
module Main where

import Data.List                      ( intercalate )
import Data.Monoid                    ( mempty )
import System.Exit                    ( ExitCode (ExitSuccess) )
import System.Process                 ( readProcessWithExitCode )
import Test.Framework                 ( Test, defaultMainWithOpts )
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit                     ( assertBool )

-- | Run all unit tests on Ogma.
main :: IO ()
main =
  defaultMainWithOpts tests mempty

-- | All unit tests for Ogma
tests :: [Test.Framework.Test]
tests =
  [
    testCase "cli-main-ok" (runErrorCode ["--help" ] True)
    -- Should pass

  , testCase "cli-main-fail" (runErrorCode ["hfdsafdkajdfaskl"] False)
    -- Should fail due to arguments being incorrect

  , testCase "cli-cmd-structs" (runErrorCode ["structs", "--help" ] True)
    -- Should pass

  , testCase "cli-cmd-structs-fail" (runErrorCode ["structs", "--incorrect-argument"] False)
    -- Should fail due to arguments being incorrect

  , testCase "cli-cmd-handlers" (runErrorCode ["handlers", "--help" ] True)
    -- Should pass

  , testCase "cli-cmd-handlers-fail" (runErrorCode ["handlers", "--incorrect-argument"] False)
    -- Should fail due to arguments being incorrect

  , testCase "cli-cmd-cfs" (runErrorCode ["cfs", "--help" ] True)
    -- Should pass

  , testCase "cli-cmd-cfs-fail" (runErrorCode ["cfs", "--incorrect-argument"] False)
    -- Should fail due to arguments being incorrect

  , testCase "cli-cmd-standalone" (runErrorCode ["standalone", "--help" ] True)
    -- Should pass

  , testCase "cli-cmd-standalone-fail" (runErrorCode ["standalone", "--incorrect-argument"] False)
    -- Should fail due to arguments being incorrect

  , testCase "cli-cmd-standalone-fcs" (parseStandaloneFCS "tests/fcs-example1.json" True)
    -- Should pass

  , testCase "cli-cmd-standalone-file-not-found" (parseStandaloneFCS "tests/file-invalid.json" False)
    -- Should fail because the file does not exist

  , testCase "cli-cmd-standalone-parse-fail-1" (parseStandaloneFCS "tests/commands-fcs-error-parsing-failed-1.json" False)
    -- Should fail because the opening bracket is [ and not {

  , testCase "cli-cmd-standalone-parse-fail-2" (parseStandaloneFCS "tests/commands-fcs-error-parsing-failed-2.json" False)
    -- Should fail because a field is missing in an external variable

  , testCase "cli-cmd-standalone-parse-fail-3" (parseStandaloneFCS "tests/commands-fcs-error-parsing-failed-3.json" False)
    -- Should fail because a field is missing in an internal variable

  , testCase "cli-cmd-standalone-fdb" (parseStandaloneFDB "tests/fdb-example1.json")
    -- Should pass

  , testCase "structs-parse-ok" (testCStructs2Copilot "tests/reduced_geofence_msgs.h" True)
    -- Should pass

  , testCase "structs-parse-fail-1" (testCStructs2Copilot "tests/reduced_geofence_msgs_bad.h" False)
    -- Should fail because a keyword is incorrect
  ]

-- | Test C struct parser for a particular file.
--
-- This test uses the Copilot backend for C header files, so it generates
-- Copilot types and instances. It may be convenient to run this action in a
-- temporary directory.
--
-- This IO action fails if any of the following are true:
--   * Ogma cannot be found in the current PATH.
--   * Ogma cannot be executed.
--   * The given file is not found or accessible.
--   * The format in the given file is incorrect.
--   * Ogma fails due to an internal error or bug.
--   * The output file cannot be created due to lack of space or permissions.
--
testCStructs2Copilot :: FilePath  -- ^ Path to a C header file with structs
                     -> Bool
                     -> IO ()
testCStructs2Copilot file success = do
    (ec, _out, _err) <- readProcessWithExitCode "ogma" args ""

    -- True if success is expected and detected, or niether expected nor
    -- detected.
    let testPass = success == (ec == ExitSuccess)

    assertBool errorMsg testPass
  where
    args     = ["structs", "--header-file-name", file]
    errorMsg = "Result of processing file " ++ file ++ " failed"

-- | Test standalone backend for a FCS format and SVM.
--
-- This test uses the standalone backend, so it generates a Copilot file. It
-- may be convenient to run this action in a temporary directory.
--
-- This IO action fails if any of the following are true:
--   * Ogma cannot be found in the current PATH.
--   * Ogma cannot be executed.
--   * The given file is not found or accessible.
--   * The format in the given file is incorrect.
--   * Ogma fails due to an internal error or bug.
--   * The output file cannot be created due to lack of space or permissions.
--
parseStandaloneFCS :: FilePath  -- ^ Path to an input file
                   -> Bool
                   -> IO ()
parseStandaloneFCS file success = do
    (ec, _out, _err) <- readProcessWithExitCode "ogma" args ""

    -- True if success is expected and detected, or niether expected nor
    -- detected.
    let testPass = success == (ec == ExitSuccess)

    assertBool errorMsg testPass
  where
    args     = ["standalone", "--file-name", file]
    errorMsg = "Parsing file " ++ file ++ " result unexpected."

-- | Test standalone backend for FDB format and Lustre.
--
-- This test uses the standalone backend, so it generates a Copilot file. It
-- may be convenient to run this action in a temporary directory.
--
-- This IO action fails if any of the following are true:
--   * Ogma cannot be found in the current PATH.
--   * Ogma cannot be executed.
--   * The given file is not found or accessible.
--   * The format in the given file is incorrect.
--   * Ogma fails due to an internal error or bug.
--   * The output file cannot be created due to lack of space or permissions.
--
parseStandaloneFDB :: FilePath  -- ^ Path to an input file
                   -> IO ()
parseStandaloneFDB file = do
    (ec, _out, _err) <- readProcessWithExitCode "ogma" args ""
    assertBool errorMsg (ec == ExitSuccess)
  where
    args     = [ "standalone", "--file-name", file, "--input-format", "fdb"
               , "--prop-format", "lustre"]
    errorMsg = "Parsing file " ++ file ++ " failed"

-- | Test ogma by running it and checking the error code.
--
-- This tests just whether ogma finishes with an error code or not. If files
-- may be generated for the command being tested, it may be convenient to run
-- this action in a temporary directory.
--
-- This IO action fails if any of the following are true:
--   * Ogma cannot be found in the current PATH.
--   * Ogma cannot be executed.
--   * The given command is not valid.
--   * Ogma fails due to an internal error or bug.
--   * Output files cannot be created due to lack of space or permissions.
--
runErrorCode :: [String] -- ^ Arguments to pass to ogma
             -> Bool
             -> IO ()
runErrorCode args success = do
    (ec, _out, _err) <- readProcessWithExitCode "ogma" args ""

    -- True if success is expected and detected, or niether expected nor
    -- detected.
    let testPass = success == (ec == ExitSuccess)

    assertBool errorMsg testPass
  where
    errorMsg = "Testing ogma's CLI parser with arguments "
             ++ intercalate "," args
             ++ " failed"
