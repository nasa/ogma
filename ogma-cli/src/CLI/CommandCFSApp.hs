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
-- | CLI interface to the CFSApp subcommand.
module CLI.CommandCFSApp
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
import Data.Location  ( Location ( LocationFile ) )
import Data.Project   ( Project (..), readProject )

-- External imports: actions or commands supported
import           Command.CFSApp ( ErrorCode )
import qualified Command.CFSApp

-- * Command

-- | Options needed to generate the cFS application.
data CommandOpts = CommandOpts
  { cFSAppProject       :: Maybe String
  , cFSAppConditionExpr :: Maybe String
  , cFSAppInputFiles    :: [String]
  , cFSAppTarget        :: String
  , cFSAppTemplateDir   :: Maybe String
  , cFSAppVarNames      :: Maybe String
  , cFSAppVarDB         :: Maybe String
  , cFSAppHandlers      :: Maybe String
  , cFSAppFormat        :: String
  , cFSAppPropFormat    :: String
  , cFSAppPropVia       :: Maybe String
  , cFSAppDiagramMode   :: String
  , cFSAppTemplateVars  :: Maybe String
  }

-- | Create <https://cfs.gsfc.nasa.gov/ NASA core Flight System> (cFS)
-- applications that subscribe to the communication bus and call Copilot when
-- new messages arrive.
--
-- This is just an uncurried version of "Command.CFSApp".
command :: CommandOpts -> IO (Result ErrorCode)
command c
  | Just p <- cFSAppProject c
  = do optE <- commandProjectOptions p c
       case optE of
         Left msg  -> return $ Error cannotReadProject msg (LocationFile p)
         Right opt -> Command.CFSApp.command opt

  | otherwise
  = Command.CFSApp.command $
      Command.CFSApp.CommandOptions
        { Command.CFSApp.commandConditionExpr = cFSAppConditionExpr c
        , Command.CFSApp.commandInputFiles    = cFSAppInputFiles c
        , Command.CFSApp.commandTargetDir     = cFSAppTarget c
        , Command.CFSApp.commandTemplateDir   = cFSAppTemplateDir c
        , Command.CFSApp.commandVariables     = cFSAppVarNames c
        , Command.CFSApp.commandVariableDB    = cFSAppVarDB c
        , Command.CFSApp.commandHandlers      = cFSAppHandlers c
        , Command.CFSApp.commandFormat        = cFSAppFormat c
        , Command.CFSApp.commandPropFormat    = cFSAppPropFormat c
        , Command.CFSApp.commandPropVia       = cFSAppPropVia c
        , Command.CFSApp.commandDiagramMode   = cFSAppDiagramMode c
        , Command.CFSApp.commandExtraVars     = cFSAppTemplateVars c
        }

-- | Produce default command options based on project settings.
commandProjectOptions :: FilePath
                      -> CommandOpts
                      -> IO (Either String Command.CFSApp.CommandOptions)
commandProjectOptions projectFile c = do
  projectE <- readProject projectFile
  return $ projectE <&> \project ->
    Command.CFSApp.CommandOptions
      { Command.CFSApp.commandConditionExpr = cFSAppConditionExpr c

      , Command.CFSApp.commandInputFiles = concat
          [ map (\(f, _, _) -> f) $ projectInputFiles project
          , cFSAppInputFiles c
          ]

      , Command.CFSApp.commandTargetDir =
          fromMaybe (cFSAppTarget c) (projectTargetDir project)

      , Command.CFSApp.commandTemplateDir =
          projectTemplateDir project <|> cFSAppTemplateDir c

      , Command.CFSApp.commandVariables =
          projectVariableFiles project <|> cFSAppVarNames c

      , Command.CFSApp.commandVariableDB =
          projectVariableDBFile project <|> cFSAppVarDB c

      , Command.CFSApp.commandHandlers =
          projectHandlerFile project <|> cFSAppHandlers c

      , Command.CFSApp.commandFormat = case projectInputFiles project of
          []            -> cFSAppFormat c
          ((_, f, _):_) -> f

      , Command.CFSApp.commandPropFormat = case projectInputFiles project of
          []            -> cFSAppPropFormat c
          ((_, _, f):_) -> f

      , Command.CFSApp.commandPropVia =
          projectCommandPropVia project <|> cFSAppPropVia c

      , Command.CFSApp.commandDiagramMode = cFSAppDiagramMode c

      , Command.CFSApp.commandExtraVars =
          projectExtraJSONFile project <|> cFSAppTemplateVars c

      }

-- * CLI

-- | cFS command description.
commandDesc :: String
commandDesc = "Generate a complete cFS/Copilot application"

-- | Subparser for the @cfs@ command, used to generate a NASA Core Flight
-- System application connected to Copilot monitors.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> optional
        ( strOption
            (  long "project"
            <> metavar "FILENAME"
            <> help strCFSAppProjectArgDesc
            )
        )
  <*> optional
        ( strOption
            (  long "condition-expr"
            <> metavar "EXPRESSION"
            <> help strCFSAppConditionExprArgDesc
            )
        )
  <*> many
        ( strOption
            (  long "input-file"
            <> metavar "FILENAME"
            <> help strCFSAppFileNameArgDesc
            )
        )
  <*> strOption
        (  long "target-dir"
        <> metavar "DIR"
        <> showDefault
        <> value "copilot-cfs-demo"
        <> help strCFSAppDirArgDesc
        )
  <*> optional
        ( strOption
            (  long "template-dir"
            <> metavar "DIR"
            <> help strCFSAppTemplateDirArgDesc
            )
        )
  <*> optional
        ( strOption
            (  long "variable-file"
            <> metavar "FILENAME"
            <> help strCFSAppVarListArgDesc
            )
        )
  <*> optional
        ( strOption
            (  long "variable-db"
            <> metavar "FILENAME"
            <> help strCFSAppVarDBArgDesc
            )
        )
  <*> optional
        ( strOption
            (  long "handlers-file"
            <> metavar "FILENAME"
            <> help strCFSAppHandlerListArgDesc
            )
        )
  <*> strOption
        (  long "input-format"
        <> short 'f'
        <> metavar "FORMAT_NAME"
        <> help strCFSAppFormatDesc
        <> showDefault
        <> value "default"
        )
  <*> strOption
        (  long "prop-format"
        <> short 'p'
        <> metavar "FORMAT_NAME"
        <> help strCFSAppPropFormatDesc
        <> showDefault
        <> value "smv"
        )
  <*> optional
        ( strOption
            (  long "parse-prop-via"
            <> metavar "COMMAND"
            <> help strCFSAppPropViaDesc
            )
        )
  <*> strOption
        (  long "mode"
        <> metavar "MODE"
        <> help strCFSAppDiagramModeDesc
        <> showDefault
        <> value "calculate"
        )
  <*> optional
        ( strOption
            (  long "template-vars"
            <> metavar "FILENAME"
            <> help strCFSAppTemplateVarsArgDesc
            )
        )

-- | Argument project to cFS app generation command.
strCFSAppProjectArgDesc :: String
strCFSAppProjectArgDesc = "Project file"

-- | Argument target directory to cFS app generation command.
strCFSAppDirArgDesc :: String
strCFSAppDirArgDesc = "Target directory"

-- | Argument template directory to cFS app generation command.
strCFSAppTemplateDirArgDesc :: String
strCFSAppTemplateDirArgDesc =
  "Directory holding cFS application source template"

-- | Argument expression to CFS app generation command.
strCFSAppConditionExprArgDesc :: String
strCFSAppConditionExprArgDesc = "Expression used as guard or trigger condition"

-- | Argument input file to CFS app generation command.
strCFSAppFileNameArgDesc :: String
strCFSAppFileNameArgDesc = "File containing input specification"

-- | Argument variable list to cFS app generation command.
strCFSAppVarListArgDesc :: String
strCFSAppVarListArgDesc =
  "File containing list of cFS variables to make accessible"

-- | Argument variable database to cFS app generation command.
strCFSAppVarDBArgDesc :: String
strCFSAppVarDBArgDesc = "File containing a DB of known cFS variables"

-- | Argument handler list to cFS app generation command.
strCFSAppHandlerListArgDesc :: String
strCFSAppHandlerListArgDesc =
  "File containing list of Copilot handlers used in the specification"

-- | Format flag description.
strCFSAppFormatDesc :: String
strCFSAppFormatDesc = "Format of the input file"

-- | Property format flag description.
strCFSAppPropFormatDesc :: String
strCFSAppPropFormatDesc = "Format of temporal or boolean properties"

-- | External command to pre-process individual properties.
strCFSAppPropViaDesc :: String
strCFSAppPropViaDesc = "Command to pre-process individual properties"

-- | Mode name flag description.
strCFSAppDiagramModeDesc :: String
strCFSAppDiagramModeDesc =
  "Mode of operation for diagrams (check, calculate, safeguard)"

-- | Argument template variables to cFS app generation command.
strCFSAppTemplateVarsArgDesc :: String
strCFSAppTemplateVarsArgDesc =
  "JSON file containing additional variables to expand in template"

-- | Error code for when a project cannot be read.
cannotReadProject :: ErrorCode
cannotReadProject = 1
