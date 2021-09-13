-- | Test ogma-core
module Main where

import Data.Monoid                    ( mempty )
import Test.Framework                 ( Test, defaultMainWithOpts )
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit                     ( assertBool )

-- Internal imports
import Command.CStructs2Copilot ( cstructs2Copilot )
import Command.Result           ( isSuccess )

-- | Run all unit tests on ogma-core.
main :: IO ()
main =
  defaultMainWithOpts tests mempty

-- | All unit tests for ogma-core.
tests :: [Test.Framework.Test]
tests =
  [
    testCase "structs-parse-ok"
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
    let testPass = success == (isSuccess result)

    assertBool errorMsg testPass
  where
    errorMsg = "The result of the transformation of the C header file "
               ++ file ++ " to Copilot struct declarations was unexpected."
