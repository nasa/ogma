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
-- | CLI interface to the ROSApp subcommand.
module CLI.CommandROSApp
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
import Options.Applicative ( Parser, help, long, metavar, many, optional, short,
                             showDefault, strOption, value )

-- External imports: handling of input projects and command results
import Command.Result ( Result (..) )
import Data.Location  ( Location ( LocationFile ) )
import Data.Project   ( Project (..), readProject )

-- External imports: actions or commands supported
import           Command.ROSApp (ErrorCode)
import qualified Command.ROSApp

-- * Command

-- | Options needed to generate the ROS application.
data CommandOpts = CommandOpts
  { rosAppProject       :: Maybe String
  , rosAppConditionExpr :: Maybe String
  , rosAppInputFiles    :: [String]
  , rosAppTarget        :: String
  , rosAppTemplateDir   :: Maybe String
  , rosAppVarNames      :: Maybe String
  , rosAppVarDB         :: Maybe String
  , rosAppHandlers      :: Maybe String
  , rosAppFormat        :: String
  , rosAppPropFormat    :: String
  , rosAppPropVia       :: Maybe String
  , rosAppTemplateVars  :: Maybe String
  , rosAppTestingApps   :: [String]
  , rosAppTestingVars   :: [String]
  }

-- | Create <https://www.ros.org/ Robot Operating System> (ROS) applications
-- that subscribe to obtain necessary data from topics and call Copilot when
-- new data arrives.
--
-- This is just a wrapper around "Command.ROSApp".
command :: CommandOpts -> IO (Result ErrorCode)
command c
    | Just p <- rosAppProject c
    = do optE <- commandProjectOptions p c
         case optE of
           Left msg  -> return $ Error cannotReadProject msg (LocationFile p)
           Right opt -> Command.ROSApp.command opt

    | otherwise
    = Command.ROSApp.command options
  where
    options = Command.ROSApp.CommandOptions
                { Command.ROSApp.commandConditionExpr = rosAppConditionExpr c
                , Command.ROSApp.commandInputFiles    = rosAppInputFiles c
                , Command.ROSApp.commandTargetDir     = rosAppTarget c
                , Command.ROSApp.commandTemplateDir   = rosAppTemplateDir c
                , Command.ROSApp.commandVariables     = rosAppVarNames c
                , Command.ROSApp.commandVariableDB    = rosAppVarDB c
                , Command.ROSApp.commandHandlers      = rosAppHandlers c
                , Command.ROSApp.commandFormat        = rosAppFormat c
                , Command.ROSApp.commandPropFormat    = rosAppPropFormat c
                , Command.ROSApp.commandPropVia       = rosAppPropVia c
                , Command.ROSApp.commandExtraVars     = rosAppTemplateVars c
                , Command.ROSApp.commandTestingApps   = appNames
                , Command.ROSApp.commandTestingVars   = rosAppTestingVars c
                }

    -- Turn the qualified app names into tuples of package and node name.
    appNames = map splitAppName $ rosAppTestingApps c

    splitAppName name = Command.ROSApp.Node package (drop 1 nodeT)
      where
        (package, nodeT) = break (== ':') name

-- | Produce default command options based on project settings.
commandProjectOptions :: FilePath
                      -> CommandOpts
                      -> IO (Either String Command.ROSApp.CommandOptions)
commandProjectOptions projectFile c = do
    projectE <- readProject projectFile
    return $ projectE <&> \project ->
      Command.ROSApp.CommandOptions
        { Command.ROSApp.commandConditionExpr = rosAppConditionExpr c

        , Command.ROSApp.commandInputFiles = concat
            [ map (\(f, _, _) -> f) $ projectInputFiles project
            , rosAppInputFiles c
            ]

        , Command.ROSApp.commandTargetDir =
            fromMaybe (rosAppTarget c) (projectTargetDir project)

        , Command.ROSApp.commandTemplateDir =
            projectTemplateDir project <|> rosAppTemplateDir c

        , Command.ROSApp.commandVariables =
            projectVariableFiles project <|> rosAppVarNames c

        , Command.ROSApp.commandVariableDB =
            projectVariableDBFile project <|> rosAppVarDB c

        , Command.ROSApp.commandHandlers =
            projectHandlerFile project <|> rosAppHandlers c

        , Command.ROSApp.commandFormat = case projectInputFiles project of
            []            -> rosAppFormat c
            ((_, f, _):_) -> f

        , Command.ROSApp.commandPropFormat =
            case projectInputFiles project of
              []            -> rosAppPropFormat c
              ((_, _, f):_) -> f

        , Command.ROSApp.commandPropVia =
            projectCommandPropVia project <|> rosAppPropVia c

        , Command.ROSApp.commandExtraVars =
            projectExtraJSONFile project <|> rosAppTemplateVars c

        , Command.ROSApp.commandTestingApps = appNames

        , Command.ROSApp.commandTestingVars = rosAppTestingVars c

        }

  where

    -- Turn the qualified app names into tuples of package and node name.
    appNames = map splitAppName $ rosAppTestingApps c

    splitAppName name = Command.ROSApp.Node package (drop 1 nodeT)
      where
        (package, nodeT) = break (== ':') name

-- * CLI

-- | ROS command description.
commandDesc :: String
commandDesc = "Generate a ROS 2 monitoring package"

-- | Subparser for the @ros@ command, used to generate a Robot Operating System
-- application connected to Copilot monitors.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> optional
        ( strOption
            (  long "project"
            <> metavar "FILENAME"
            <> help strROSAppProjectArgDesc
            )
        )
  <*> optional
        ( strOption
            (  long "condition-expr"
            <> metavar "EXPRESSION"
            <> help strROSAppConditionExprArgDesc
            )
        )
  <*> many
        ( strOption
            (  long "input-file"
            <> metavar "FILENAME"
            <> help strROSAppFileNameArgDesc
            )
        )
  <*> strOption
        (  long "target-dir"
        <> metavar "DIR"
        <> showDefault
        <> value "ros"
        <> help strROSAppDirArgDesc
        )
  <*> optional
        ( strOption
            (  long "template-dir"
            <> metavar "DIR"
            <> help strROSAppTemplateDirArgDesc
            )
        )
  <*> optional
        ( strOption
            (  long "variable-file"
            <> metavar "FILENAME"
            <> help strROSAppVarListArgDesc
            )
        )
  <*> optional
        ( strOption
            (  long "variable-db"
            <> metavar "FILENAME"
            <> help strROSAppVarDBArgDesc
            )
        )
  <*> optional
        ( strOption
            (  long "handlers-file"
            <> metavar "FILENAME"
            <> help strROSAppHandlerListArgDesc
            )
        )
  <*> strOption
        (  long "input-format"
        <> short 'f'
        <> metavar "FORMAT_NAME"
        <> help strROSAppFormatDesc
        <> showDefault
        <> value "default"
        )
  <*> strOption
        (  long "prop-format"
        <> short 'p'
        <> metavar "FORMAT_NAME"
        <> help strROSAppPropFormatDesc
        <> showDefault
        <> value "smv"
        )
  <*> optional
        ( strOption
            (  long "parse-prop-via"
            <> metavar "COMMAND"
            <> help strROSAppPropViaDesc
            )
        )
  <*> optional
        ( strOption
            (  long "template-vars"
            <> metavar "FILENAME"
            <> help strROSAppTemplateVarsArgDesc
            )
        )
  <*> many (strOption
              (  long "testing-app"
              <> metavar "package:node"
              <> help strROSAppROSNodesTestingListArgDesc
              )
           )
  <*> many (strOption
              (  long "testing-vars"
              <> metavar "variable_name"
              <> showDefault
              <> help strROSAppHandlerListArgDesc
              )
           )

-- | Argument project to ROS app generation command.
strROSAppProjectArgDesc :: String
strROSAppProjectArgDesc = "Project file"

-- | Argument target directory to ROS app generation command.
strROSAppDirArgDesc :: String
strROSAppDirArgDesc = "Target directory"

-- | Argument template directory to ROS app generation command.
strROSAppTemplateDirArgDesc :: String
strROSAppTemplateDirArgDesc =
  "Directory holding ROS application source template"

-- | Argument expression to ROS app generation command.
strROSAppConditionExprArgDesc :: String
strROSAppConditionExprArgDesc = "Expression used as guard or trigger condition"

-- | Argument input file to ROS app generation command.
strROSAppFileNameArgDesc :: String
strROSAppFileNameArgDesc = "File containing input specification"

-- | Argument variable list to ROS app generation command.
strROSAppVarListArgDesc :: String
strROSAppVarListArgDesc =
  "File containing list of ROS variables to make accessible"

-- | Argument variable database to ROS app generation command.
strROSAppVarDBArgDesc :: String
strROSAppVarDBArgDesc = "File containing a DB of known ROS variables"

-- | Argument handler list to ROS app generation command.
strROSAppHandlerListArgDesc :: String
strROSAppHandlerListArgDesc =
  "File containing list of Copilot handlers used in the specification"

-- | Format flag description.
strROSAppFormatDesc :: String
strROSAppFormatDesc = "Format of the input file"

-- | Property format flag description.
strROSAppPropFormatDesc :: String
strROSAppPropFormatDesc = "Format of temporal or boolean properties"

-- | External command to pre-process individual properties.
strROSAppPropViaDesc :: String
strROSAppPropViaDesc = "Command to pre-process individual properties"

-- | Additional template variable file flag description.
strROSAppTemplateVarsArgDesc :: String
strROSAppTemplateVarsArgDesc =
  "JSON file containing additional variables to expand in template"

-- | Argument packages to tested list to ROS app generation command.
strROSAppROSNodesTestingListArgDesc :: String
strROSAppROSNodesTestingListArgDesc =
  "Turn on ROS 2 package node during testing"

-- | Argument variables to be tested list to ROS app generation command.
strROSAppVarsTestingListArgDesc :: String
strROSAppVarsTestingListArgDesc =
  "Limit random input generation to these variables"

-- | Error code for when a project cannot be read.
cannotReadProject :: ErrorCode
cannotReadProject = 1
