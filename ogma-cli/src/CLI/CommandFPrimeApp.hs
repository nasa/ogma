-- Copyright 2022 United States Government as represented by the Administrator
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
-- | CLI interface to the FPrimeApp subcommand.
module CLI.CommandFPrimeApp
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
import Options.Applicative ( Parser, help, long, metavar, optional, short,
                             showDefault, strOption, value )

-- External imports: command results
import Command.Result ( Result )

-- External imports: actions or commands supported
import           Command.FPrimeApp (ErrorCode)
import qualified Command.FPrimeApp

-- * Command

-- | Options needed to generate the FPrime component.
data CommandOpts = CommandOpts
  { fprimeAppConditionExpr  :: Maybe String
  , fprimeAppInputFile    :: Maybe String
  , fprimeAppTarget       :: String
  , fprimeAppTemplateDir  :: Maybe String
  , fprimeAppVariables    :: Maybe String
  , fprimeAppVarDB        :: Maybe String
  , fprimeAppHandlers     :: Maybe String
  , fprimeAppFormat       :: String
  , fprimeAppPropFormat   :: String
  , fprimeAppPropVia      :: Maybe String
  , fprimeAppTemplateVars :: Maybe String
  }

-- | Create <https://github.com/nasa/fprime FPrime> component that subscribe
-- to obtain necessary data from the bus and call Copilot when new data
-- arrives.
--
-- This is just a wrapper around "Command.fprimeApp".
command :: CommandOpts -> IO (Result ErrorCode)
command c = Command.FPrimeApp.command options
  where
    options =
      Command.FPrimeApp.CommandOptions
        { Command.FPrimeApp.commandConditionExpr = fprimeAppConditionExpr c
        , Command.FPrimeApp.commandInputFile   = fprimeAppInputFile c
        , Command.FPrimeApp.commandTargetDir   = fprimeAppTarget c
        , Command.FPrimeApp.commandTemplateDir = fprimeAppTemplateDir c
        , Command.FPrimeApp.commandVariables   = fprimeAppVariables c
        , Command.FPrimeApp.commandVariableDB  = fprimeAppVarDB c
        , Command.FPrimeApp.commandHandlers    = fprimeAppHandlers c
        , Command.FPrimeApp.commandFormat      = fprimeAppFormat c
        , Command.FPrimeApp.commandPropFormat  = fprimeAppPropFormat c
        , Command.FPrimeApp.commandPropVia     = fprimeAppPropVia c
        , Command.FPrimeApp.commandExtraVars   = fprimeAppTemplateVars c
        }

-- * CLI

-- | FPrime command description
commandDesc :: String
commandDesc = "Generate a complete F' monitoring component"

-- | Subparser for the @fprime@ command, used to generate an FPrime component
-- connected to Copilot monitors.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> optional
        ( strOption
            (  long "condition-expr"
            <> metavar "EXPRESSION"
            <> help strFPrimeAppConditionExprArgDesc
            )
        )
  <*> optional
        ( strOption
            (  long "input-file"
            <> metavar "FILENAME"
            <> help strFPrimeAppFileNameArgDesc
            )
        )
  <*> strOption
        (  long "app-target-dir"
        <> metavar "DIR"
        <> showDefault
        <> value "fprime"
        <> help strFPrimeAppDirArgDesc
        )
  <*> optional
        ( strOption
            (  long "app-template-dir"
            <> metavar "DIR"
            <> help strFPrimeAppTemplateDirArgDesc
            )
        )
  <*> optional
        ( strOption
            (  long "variable-file"
            <> metavar "FILENAME"
            <> help strFPrimeAppVarListArgDesc
            )
        )
  <*> optional
        ( strOption
            (  long "variable-db"
            <> metavar "FILENAME"
            <> help strFPrimeAppVarDBArgDesc
            )
        )
  <*> optional
        ( strOption
            (  long "handlers-file"
            <> metavar "FILENAME"
            <> help strFPrimeAppHandlerListArgDesc
            )
        )
  <*> strOption
        (  long "input-format"
        <> short 'f'
        <> metavar "FORMAT_NAME"
        <> help strFPrimeAppFormatDesc
        <> showDefault
        <> value "fcs"
        )
  <*> strOption
        (  long "prop-format"
        <> short 'p'
        <> metavar "FORMAT_NAME"
        <> help strFPrimeAppPropFormatDesc
        <> showDefault
        <> value "smv"
        )
  <*> optional
        ( strOption
            (  long "parse-prop-via"
            <> metavar "COMMAND"
            <> help strFPrimeAppPropViaDesc
            )
        )
  <*> optional
        ( strOption
            (  long "template-vars"
            <> metavar "FILENAME"
            <> help strFPrimeAppTemplateVarsArgDesc
            )
        )

-- | Argument target directory to FPrime component generation command
strFPrimeAppDirArgDesc :: String
strFPrimeAppDirArgDesc = "Target directory"

-- | Argument template directory to FPrime component generation command
strFPrimeAppTemplateDirArgDesc :: String
strFPrimeAppTemplateDirArgDesc =
  "Directory holding F' component source template"

-- | Argument expression to FPrime app generation command.
strFPrimeAppConditionExprArgDesc :: String
strFPrimeAppConditionExprArgDesc = "Expression used as guard or trigger condition"

-- | Argument input file to FPrime component generation command
strFPrimeAppFileNameArgDesc :: String
strFPrimeAppFileNameArgDesc =
  "File containing input specification"

-- | Argument variable list to FPrime component generation command
strFPrimeAppVarListArgDesc :: String
strFPrimeAppVarListArgDesc =
  "File containing list of F' variables to make accessible"

-- | Argument variable database to FPrime component generation command
strFPrimeAppVarDBArgDesc :: String
strFPrimeAppVarDBArgDesc =
  "File containing a DB of known F' variables"

-- | Argument handler list to FPrime component generation command
strFPrimeAppHandlerListArgDesc :: String
strFPrimeAppHandlerListArgDesc =
  "File containing list of Copilot handlers used in the specification"

-- | Format flag description.
strFPrimeAppFormatDesc :: String
strFPrimeAppFormatDesc = "Format of the input file"

-- | Property format flag description.
strFPrimeAppPropFormatDesc :: String
strFPrimeAppPropFormatDesc = "Format of temporal or boolean properties"

-- | External command to pre-process individual properties.
strFPrimeAppPropViaDesc :: String
strFPrimeAppPropViaDesc =
  "Command to pre-process individual properties"

-- | Additional template variable file flag description.
strFPrimeAppTemplateVarsArgDesc :: String
strFPrimeAppTemplateVarsArgDesc =
  "JSON file containing additional variables to expand in template"
