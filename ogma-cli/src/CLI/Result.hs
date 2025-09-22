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
-- | Process a result of a command in a way suitable for command-line
-- interaction.
module CLI.Result where

-- External imports
import System.Exit ( ExitCode (ExitFailure, ExitSuccess), exitWith )
import System.IO   ( hPutStrLn, stderr )

-- External imports: command results
import Command.Result ( Result (Error, Success) )
import Data.Location  ( Location (..) )

{- HLINT ignore "Use exitSuccess" -}
-- | Process a result, report any pending messages, and exit with an error code
-- if necessary.
processResult :: Result Int -> IO ()
processResult Success            = exitWith ExitSuccess
processResult (Error ec msg loc) = do
    hPutStrLn stderr $ showLocation loc ++ "error: " ++ msg
    exitWith (ExitFailure ec)
  where
    -- | Show locations in a standard way in user messages.
    showLocation :: Location -> String
    showLocation LocationNothing        = "<no location info>: "
    showLocation (LocationFile f)       = f ++ ": "
    showLocation (LocationFileLine f l) = f ++ ":" ++ show l ++ ": "
    showLocation (LocationFileLC f l c) = f ++ ":" ++ show l ++ ":" ++ show c ++ ": "
