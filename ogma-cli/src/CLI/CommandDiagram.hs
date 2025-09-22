-- Copyright 2024 United States Government as represented by the Administrator
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
-- | CLI interface to the diagram subcommand.
module CLI.CommandDiagram
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
import Options.Applicative (Parser, help, long, metavar, optional, short,
                            showDefault, strOption, value)

-- External imports: command results
import Command.Result ( Result(..) )
import Data.Location  ( Location(..) )

-- External imports: actions or commands supported
import           Command.Diagram (diagram)
import qualified Command.Diagram

-- * Command

-- | Options to generate Copilot from diagram.
data CommandOpts = CommandOpts
  { diagramTargetDir   :: FilePath
  , diagramTemplateDir :: Maybe FilePath
  , diagramFileName    :: FilePath
  , diagramFormat      :: String
  , diagramPropFormat  :: String
  , diagramTarget      :: String
  , diagramMode        :: String
  , diagramStateVar    :: String
  , diagramInputVar    :: String
  }

-- | Transform an input diagram into a Copilot specification.
command :: CommandOpts -> IO (Result ErrorCode)
command c
  | Nothing <- diagramFormatP
  = return $
      Error
        ecFormatError
        "The diagram format specified is incorrect"
        LocationNothing

  | Nothing <- diagramModeP
  = return $
      Error ecModeError "The mode specified is incorrect" LocationNothing

  | Nothing <- diagramPropFormatP
  = return $
      Error
        ecPropFormatError
        "The format specified for transitions or edge properties is incorrect"
        LocationNothing

  | Just mode       <- diagramModeP
  , Just format     <- diagramFormatP
  , Just propFormat <- diagramPropFormatP
  = do let internalCommandOpts :: Command.Diagram.DiagramOptions
           internalCommandOpts = Command.Diagram.DiagramOptions
             { Command.Diagram.diagramTargetDir   = diagramTargetDir c
             , Command.Diagram.diagramTemplateDir = diagramTemplateDir c
             , Command.Diagram.diagramFormat      = format
             , Command.Diagram.diagramPropFormat  = propFormat
             , Command.Diagram.diagramFilename    = diagramTarget c
             , Command.Diagram.diagramMode        = mode
             , Command.Diagram.diagramStateVar    = diagramStateVar c
             , Command.Diagram.diagramInputVar    = diagramInputVar c
             }
       diagram (diagramFileName c) internalCommandOpts
  where
    diagramFormatP     = parseDiagramFormat (diagramFormat c)
    diagramModeP       = parseDiagramMode (diagramMode c)
    diagramPropFormatP = parseDiagramPropFormat (diagramPropFormat c)

    parseDiagramFormat :: String
                       -> Maybe Command.Diagram.DiagramFormat
    parseDiagramFormat "dot"      = Just Command.Diagram.Dot
    parseDiagramFormat "graphviz" = Just Command.Diagram.Dot
    parseDiagramFormat "mermaid"  = Just Command.Diagram.Mermaid
    parseDiagramFormat _          = Nothing

    parseDiagramMode :: String -> Maybe Command.Diagram.DiagramMode
    parseDiagramMode "check"     = Just Command.Diagram.CheckState
    parseDiagramMode "calculate" = Just Command.Diagram.ComputeState
    parseDiagramMode "safeguard" = Just Command.Diagram.CheckMoves
    parseDiagramMode _           = Nothing

    parseDiagramPropFormat :: String -> Maybe Command.Diagram.DiagramPropFormat
    parseDiagramPropFormat "lustre"   = Just Command.Diagram.Lustre
    parseDiagramPropFormat "cocospec" = parseDiagramPropFormat "lustre"
    parseDiagramPropFormat "inputs"   = Just Command.Diagram.Inputs
    parseDiagramPropFormat "literal"  = Just Command.Diagram.Literal
    parseDiagramPropFormat "smv"      = Just Command.Diagram.SMV
    parseDiagramPropFormat _          = Nothing

-- * CLI

-- | Command description for CLI help.
commandDesc :: String
commandDesc =
  "Generate a monitor from state machine diagram"

-- | Subparser for the @diagram@ command, used to generate a Copilot
-- specification from an input diagram file.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> strOption
        (  long "target-dir"
        <> metavar "DIR"
        <> showDefault
        <> value "copilot"
        <> help strDiagramTargetDirDesc
        )
  <*> optional
        ( strOption
            (  long "template-dir"
            <> metavar "DIR"
            <> help strDiagramTemplateDirArgDesc
            )
        )
  <*> strOption
        (  long "file-name"
        <> metavar "FILENAME"
        <> help strDiagramFilenameDesc
        )
  <*> strOption
        (  long "input-format"
        <> short 'f'
        <> metavar "FORMAT_NAME"
        <> help strDiagramFormatDesc
        <> showDefault
        <> value "mermaid"
        )
  <*> strOption
        (  long "prop-format"
        <> short 'p'
        <> metavar "FORMAT_NAME"
        <> help strDiagramPropFormatDesc
        <> showDefault
        <> value "inputs"
        )
  <*> strOption
        (  long "target-file-name"
        <> metavar "FILENAME"
        <> help strDiagramTargetDesc
        <> showDefault
        <> value "monitor"
        )
  <*> strOption
        (  long "mode"
        <> metavar "MODE"
        <> help strDiagramModeDesc
        <> showDefault
        <> value "check"
        )
  <*> strOption
        (  long "state-var"
        <> metavar "NAME"
        <> help strDiagramStateVarDesc
        <> showDefault
        <> value "state"
        )
  <*> strOption
        (  long "input-var"
        <> metavar "NAME"
        <> help strDiagramInputVarDesc
        <> showDefault
        <> value "input"
        )

-- | Target dir flag description.
strDiagramTargetDirDesc :: String
strDiagramTargetDirDesc = "Target directory"

-- | Template dir flag description.
strDiagramTemplateDirArgDesc :: String
strDiagramTemplateDirArgDesc = "Directory holding target source template"

-- | Filename flag description.
strDiagramFilenameDesc :: String
strDiagramFilenameDesc = "File with diagram source"

-- | Format flag description.
strDiagramFormatDesc :: String
strDiagramFormatDesc = "Format of the input file"

-- | Property format flag description.
strDiagramPropFormatDesc :: String
strDiagramPropFormatDesc =
  "Format of temporal or boolean properties associated to diagram edges"

-- | Target file name flag description.
strDiagramTargetDesc :: String
strDiagramTargetDesc =
  "Filename prefix for monitoring files in target language"

-- | Mode name flag description.
strDiagramModeDesc :: String
strDiagramModeDesc =
  "Mode of operation (check, calculate, safeguard)"

strDiagramInputVarDesc :: String
strDiagramInputVarDesc =
  "Name of the input variable"

strDiagramStateVarDesc :: String
strDiagramStateVarDesc =
  "Name of the state variable"

-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error code used is 1 for user error.
type ErrorCode = Int

-- | Error: unknown diagram format.
ecFormatError :: ErrorCode
ecFormatError = 2

-- | Error: unknown operation mode.
ecModeError :: ErrorCode
ecModeError = 3

-- | Error: unknown property format.
ecPropFormatError :: ErrorCode
ecPropFormatError = 4
