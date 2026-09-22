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
import Control.Applicative ( (<|>) )
import Data.Functor        ( (<&>) )
import Data.Maybe          ( fromMaybe )
import Options.Applicative ( Parser, help, long, many, metavar, optional,
                             short, showDefault, strOption, value )

-- External imports: handling of input projects and command results
import Command.Result ( Result (..) )
import Data.Location  ( Location (LocationFile) )
import Data.Project   ( Project (..), readProject )

-- External imports: actions or commands supported
import           Command.FPrimeApp (ErrorCode)
import qualified Command.FPrimeApp

-- * Command

-- | Options needed to generate the F Prime component.
data CommandOpts = CommandOpts
  { fprimeAppProject       :: Maybe String
  , fprimeAppConditionExpr :: Maybe String
  , fprimeAppInputFiles    :: [String]
  , fprimeAppTarget        :: String
  , fprimeAppTemplateDir   :: Maybe String
  , fprimeAppVariables     :: Maybe String
  , fprimeAppVarDB         :: Maybe String
  , fprimeAppHandlers      :: Maybe String
  , fprimeAppFormat        :: String
  , fprimeAppPropFormat    :: String
  , fprimeAppPropVia       :: Maybe String
  , fprimeAppTemplateVars  :: Maybe String
  }

-- | Create <https://github.com/nasa/fprime F Prime> component that subscribe
-- to obtain necessary data from the bus and call Copilot when new data
-- arrives.
--
-- This is just a wrapper around "Command.fprimeApp".
command :: CommandOpts -> IO (Result ErrorCode)
command c
    | Just p <- fprimeAppProject c
    = do optE <- commandProjectOptions p c
         case optE of
           Left msg  -> return $ Error cannotReadProject msg (LocationFile p)
           Right opt -> Command.FPrimeApp.command opt

    | otherwise
    = Command.FPrimeApp.command options
  where
    options =
      Command.FPrimeApp.CommandOptions
        { Command.FPrimeApp.commandConditionExpr = fprimeAppConditionExpr c
        , Command.FPrimeApp.commandInputFiles    = fprimeAppInputFiles c
        , Command.FPrimeApp.commandTargetDir     = fprimeAppTarget c
        , Command.FPrimeApp.commandTemplateDir   = fprimeAppTemplateDir c
        , Command.FPrimeApp.commandVariables     = fprimeAppVariables c
        , Command.FPrimeApp.commandVariableDB    = fprimeAppVarDB c
        , Command.FPrimeApp.commandHandlers      = fprimeAppHandlers c
        , Command.FPrimeApp.commandFormat        = fprimeAppFormat c
        , Command.FPrimeApp.commandPropFormat    = fprimeAppPropFormat c
        , Command.FPrimeApp.commandPropVia       = fprimeAppPropVia c
        , Command.FPrimeApp.commandExtraVars     = fprimeAppTemplateVars c
        }

-- | Produce default command options based on project settings.
commandProjectOptions :: FilePath
                      -> CommandOpts
                      -> IO (Either String Command.FPrimeApp.CommandOptions)
commandProjectOptions projectFile c = do
  projectE <- readProject projectFile
  return $ projectE <&> \project ->
    Command.FPrimeApp.CommandOptions
      { Command.FPrimeApp.commandConditionExpr = fprimeAppConditionExpr c

      , Command.FPrimeApp.commandInputFiles = concat
          [ map (\(f, _, _) -> f) $ projectInputFiles project
          , fprimeAppInputFiles c
          ]

      , Command.FPrimeApp.commandTargetDir =
          fromMaybe (fprimeAppTarget c) (projectTargetDir project)

      , Command.FPrimeApp.commandTemplateDir =
          projectTemplateDir project <|> fprimeAppTemplateDir c

      , Command.FPrimeApp.commandVariables =
          projectVariableFiles project <|> fprimeAppVariables c

      , Command.FPrimeApp.commandVariableDB =
          projectVariableDBFile project <|> fprimeAppVarDB c

      , Command.FPrimeApp.commandHandlers =
          projectHandlerFile project <|> fprimeAppHandlers c

      , Command.FPrimeApp.commandFormat = case projectInputFiles project of
          []            -> fprimeAppFormat c
          ((_, f, _):_) -> f

      , Command.FPrimeApp.commandPropFormat = case projectInputFiles project of
          []            -> fprimeAppPropFormat c
          ((_, _, f):_) -> f

      , Command.FPrimeApp.commandPropVia =
          projectCommandPropVia project <|> fprimeAppPropVia c

      , Command.FPrimeApp.commandExtraVars =
          projectExtraJSONFile project <|> fprimeAppTemplateVars c

      }

-- * CLI

-- | F Prime command description.
commandDesc :: String
commandDesc = "Generate a complete F' monitoring component"

-- | Subparser for the @fprime@ command, used to generate an F Prime component
-- connected to Copilot monitors.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> optional
        ( strOption
            (  long "project"
            <> metavar "FILENAME"
            <> help strFPrimeAppProjectArgDesc
            )
        )
  <*> optional
        ( strOption
            (  long "condition-expr"
            <> metavar "EXPRESSION"
            <> help strFPrimeAppConditionExprArgDesc
            )
        )
  <*> many
        ( strOption
            (  long "input-file"
            <> metavar "FILENAME"
            <> help strFPrimeAppFileNameArgDesc
            )
        )
  <*> strOption
        (  long "target-dir"
        <> metavar "DIR"
        <> showDefault
        <> value "fprime"
        <> help strFPrimeAppDirArgDesc
        )
  <*> optional
        ( strOption
            (  long "template-dir"
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
        <> value "default"
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

-- | Argument project to F Prime app generation command.
strFPrimeAppProjectArgDesc :: String
strFPrimeAppProjectArgDesc = "Project file"

-- | Argument target directory to F Prime component generation command.
strFPrimeAppDirArgDesc :: String
strFPrimeAppDirArgDesc = "Target directory"

-- | Argument template directory to F Prime component generation command.
strFPrimeAppTemplateDirArgDesc :: String
strFPrimeAppTemplateDirArgDesc =
  "Directory holding F' component source template"

-- | Argument expression to F Prime app generation command.
strFPrimeAppConditionExprArgDesc :: String
strFPrimeAppConditionExprArgDesc =
  "Expression used as guard or trigger condition"

-- | Argument input file to F Prime component generation command.
strFPrimeAppFileNameArgDesc :: String
strFPrimeAppFileNameArgDesc = "File containing input specification"

-- | Argument variable list to F Prime component generation command.
strFPrimeAppVarListArgDesc :: String
strFPrimeAppVarListArgDesc =
  "File containing list of F' variables to make accessible"

-- | Argument variable database to F Prime component generation command.
strFPrimeAppVarDBArgDesc :: String
strFPrimeAppVarDBArgDesc = "File containing a DB of known F' variables"

-- | Argument handler list to F Prime component generation command.
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
strFPrimeAppPropViaDesc = "Command to pre-process individual properties"

-- | Additional template variable file flag description.
strFPrimeAppTemplateVarsArgDesc :: String
strFPrimeAppTemplateVarsArgDesc =
  "JSON file containing additional variables to expand in template"

-- | Error code for when a project cannot be read.
cannotReadProject :: ErrorCode
cannotReadProject = 1
