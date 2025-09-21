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
-- | Ogma: Tool to interoperate between <https://cfs.gsfc.nasa.gov/ Copilot>
-- and other languages.
--
-- Ogma is a tool to facilitate integration of safe runtime monitors into other
-- systems. It takes information from a system created in a language (e.g.,
-- Lustre) and produces specifications for the runtime verification
-- framework <https://cfs.gsfc.nasa.gov/ Copilot>. Currently, features
-- supported are:
--
-- * Translation properties defined in structured natural language into
-- corresponding expressions in Copilot.
--
-- * Translation of C headers declaring structs into the corresponding Copilot
-- Struct definitions.
--
-- * Translation of C headers declaring structs into CFS message handlers that
-- copy data in global variables.
--
-- * Generate NASA core Flight System (cFS) applications for runtime monitoring
-- using Copilot.
--
-- * Generate Robot Operating System (ROS) applications for runtime monitoring
-- using Copilot.
--
-- * Generate F' (FPrime) components for runtime monitoring using Copilot.
--
-- More information can be obtained by calling ogma with the argument @--help@.
module Main
    ( main )
  where

-- External imports
import Options.Applicative (ParserInfo, execParser, fullDesc, header, helper,
                            info, progDesc, (<**>))

-- Internal imports: CLI parsing, handling, and processing of results.
import CLI.CommandTop ( CommandOpts, command, commandDesc, commandOptsParser )
import CLI.Result     ( processResult )

-- | Ogma: Helper tool to interoperate between Copilot and other languages.
main :: IO ()
main = execParser fullCLIOpts >>= command >>= processResult

-- | Full program options.
fullCLIOpts :: ParserInfo CommandOpts
fullCLIOpts = info (commandOptsParser <**> helper)
  (  fullDesc
  <> progDesc commandDesc
  <> header strProgramSummary
  )

-- | Short program description
strProgramSummary :: String
strProgramSummary =
  "ogma - an anything-to-Copilot application generator"
