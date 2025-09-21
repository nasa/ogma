{-# LANGUAGE OverloadedStrings #-}
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
-- | CLI interface to the Overview subcommand.
module CLI.CommandOverview
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
import           Data.Aeson          (toJSON)
import qualified Data.Text.Lazy      as T
import qualified Data.Text.Lazy.IO   as T
import           Options.Applicative (Parser, help, long, metavar, optional,
                                      short, showDefault, strOption, value)
import           Text.Microstache

-- External imports: command results
import Command.Result ( Result(..) )

-- External imports: actions or commands supported
import           Command.Overview (ErrorCode)
import qualified Command.Overview

-- * Command

-- | Options to generate an overview from the input specification(s).
data CommandOpts = CommandOpts
  { overviewFileName    :: FilePath
  , overviewFormat      :: String
  , overviewPropFormat  :: String
  , overviewPropVia     :: Maybe String
  }

-- | Print an overview of the input specification(s).
command :: CommandOpts -> IO (Result ErrorCode)
command c = do
    (mOutput, result) <-
      Command.Overview.command (overviewFileName c) internalCommandOpts

    case (mOutput, outputString) of
      (Just output, Right template) ->
         T.putStr $ renderMustache template (toJSON output)
      _ -> putStrLn "Error"
    return result

  where
    internalCommandOpts :: Command.Overview.CommandOptions
    internalCommandOpts = Command.Overview.CommandOptions
      { Command.Overview.commandFormat      = overviewFormat c
      , Command.Overview.commandPropFormat  = overviewPropFormat c
      , Command.Overview.commandPropVia     = overviewPropVia c
      }

    outputString = compileMustacheText "output" $ T.unlines
      [ "The file has:"
      , " - {{commandExternalVariables}} external variables."
      , " - {{commandInternalVariables}} internal variables."
      , " - {{commandRequirements}} requirements."
      ]

-- * CLI

-- | Command description for CLI help.
commandDesc :: String
commandDesc = "Generate an overview of the input specification(s)"

-- | Subparser for the @overview@ command, used to generate an overview
-- of the input specifications.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> strOption
        (  long "file-name"
        <> metavar "FILENAME"
        <> help strOverviewFilenameDesc
        )
  <*> strOption
        (  long "input-format"
        <> short 'f'
        <> metavar "FORMAT_NAME"
        <> help strOverviewFormatDesc
        <> showDefault
        <> value "fcs"
        )
  <*> strOption
        (  long "prop-format"
        <> short 'p'
        <> metavar "FORMAT_NAME"
        <> help strOverviewPropFormatDesc
        <> showDefault
        <> value "smv"
        )
  <*> optional
        ( strOption
            (  long "parse-prop-via"
            <> metavar "COMMAND"
            <> help strOverviewPropViaDesc
            )
        )

-- | Filename flag description.
strOverviewFilenameDesc :: String
strOverviewFilenameDesc = "File with properties or requirements"

-- | Format flag description.
strOverviewFormatDesc :: String
strOverviewFormatDesc = "Format of the input file"

-- | Property format flag description.
strOverviewPropFormatDesc :: String
strOverviewPropFormatDesc = "Format of temporal or boolean properties"

-- | External command to pre-process individual properties.
strOverviewPropViaDesc :: String
strOverviewPropViaDesc =
  "Command to pre-process individual properties"
