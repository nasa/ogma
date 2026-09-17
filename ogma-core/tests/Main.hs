{- HLint ignore "Reduce duplication" -}
-- | Test ogma-core.
module Main where

import Data.List                      ( isPrefixOf, tails )
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
main = defaultMainWithOpts tests mempty

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

  , testCase "standalone-cmd-fcs-merge-externs"
      (testStandaloneMerge
         ["tests/merge-spec-1.json", "tests/merge-spec-2.json"]
         [("sensor_a", "Bool"), ("sensor_b", "Bool")]
      )
    -- Should pass: the monitor generated from two input files must declare
    -- the external variables of both files exactly once, with their types

  , testCase "standalone-cmd-fcs-merge-externs-three"
      (testStandaloneMerge
         [ "tests/merge-spec-1.json"
         , "tests/merge-spec-2.json"
         , "tests/merge-spec-3.json"
         ]
         [ ("sensor_a", "Bool")
         , ("sensor_b", "Bool")
         , ("temperature", "Int64")
         ]
      )
    -- Should pass: the external variables of three input files, of mixed
    -- types (bool, bool, int), must all survive the chain of merges

  , testCase "standalone-cmd-fcs-merge-externs-order"
      (testStandaloneMerge
         ["tests/merge-spec-2.json", "tests/merge-spec-1.json"]
         [("sensor_b", "Bool"), ("sensor_a", "Bool")]
      )
    -- Should pass: swapping the order of the input files must not change
    -- which external variables are declared
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
                 { commandConditionExpr = Nothing
                 , commandInputFiles  = [ file ]
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
                 { commandConditionExpr = Nothing
                 , commandInputFiles  = [ file ]
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

-- | Test standalone backend with multiple input files.
--
-- This test runs the standalone backend with several FCS input files, and
-- checks that the generated Copilot monitor declares an extern stream, with
-- the declared type, exactly once for every external variable given. External
-- variables come from all input files, so this test fails if merging the
-- specifications drops or duplicates any of them. (A variable that is dropped
-- but still used in a requirement is re-declared by Ogma without its type, so
-- checking that the name appears is not enough.)
--
-- This IO action fails if any of the following are true:
--   * Any of the given files is not found or accessible.
--   * The format in any of the given files is incorrect.
--   * The generated monitor does not declare every external variable
--     exactly once with its type.
--   * Ogma fails due to an internal error or bug.
testStandaloneMerge :: [FilePath]          -- ^ Paths to input files
                    -> [(String, String)]  -- ^ External variables and their
                                           --   Copilot types expected in the
                                           --   monitor
                    -> IO ()
testStandaloneMerge files externs = do
    targetDir <- getTemporaryDirectory
    let opts = CommandOptions
                 { commandConditionExpr = Nothing
                 , commandInputFiles  = files
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

    assertBool errorMsg (isSuccess result)

    monitor <- readFile (targetDir ++ "/Copilot.hs")

    -- Number of times each external variable is declared, with its type, as
    -- an extern stream in the generated monitor. Each must be exactly one.
    let declaration (v, t) = v ++ " :: Stream (" ++ t ++ ")\n"
                             ++ v ++ " = extern " ++ show v ++ " Nothing\n"
        occurrences s      = length $ filter (s `isPrefixOf`) $ tails monitor
        wrong              = [ v | e@(v, _) <- externs
                                 , occurrences (declaration e) /= 1 ]

    assertBool (wrongMsg wrong) (null wrong)
  where
    errorMsg = "The result of the transformation of input files "
               ++ show files ++ " to Copilot was unexpected."

    wrongMsg vs = "The monitor generated from input files " ++ show files
                  ++ " does not declare the external variables " ++ show vs
                  ++ " exactly once with their types."
