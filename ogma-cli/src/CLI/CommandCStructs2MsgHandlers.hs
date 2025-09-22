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
-- | CLI interface to the CStructs2Copilot subcommand
module CLI.CommandCStructs2MsgHandlers
    (
      -- * Direct command access
      command
    , CommandOpts
    , ErrorCode

      -- * CLI
    , commandDesc
    , commandOptsParser
    )
  where

-- External imports
import Options.Applicative ( Parser, help, long, metavar, strOption )

-- External imports: command results
import Command.Result ( Result )

-- External imports: actions or commands supported
import Command.CStructs2MsgHandlers ( ErrorCode, cstructs2MsgHandlers )

-- * Command

-- | Options to generate message handlers from C struct definitions.
newtype CommandOpts = CommandOpts
  { msgHandlersFileName :: FilePath }

-- | Generate C methods that process NASA Core Flight System messages dealing
-- with the structs defined in a header file.
--
-- This is just an uncurried version of "Command.CStructs2MsgHandlers".
command :: CommandOpts -> IO (Result ErrorCode)
command c = cstructs2MsgHandlers (msgHandlersFileName c)

-- * CLI

-- | Command description for CLI help.
commandDesc :: String
commandDesc = "Generate message handlers from C structs"

-- | Subparser for the @handlers@ command, used to generate message handers
-- from C structs.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> strOption
        (  long "header-file-name"
        <> metavar "FILENAME"
        <> help strMsgHandlersHeaderArgDesc
        )

-- | Argument C header file to handler generation command
strMsgHandlersHeaderArgDesc :: String
strMsgHandlersHeaderArgDesc = "C header file with struct definitions"
