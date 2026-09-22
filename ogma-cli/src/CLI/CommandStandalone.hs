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
-- | CLI interface to the Standalone subcommand.
module CLI.CommandStandalone
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
import Control.Applicative ((<|>))
import Data.Functor        ((<&>))
import Data.Maybe          (fromMaybe)
import Options.Applicative (Parser, help, long, many, metavar, optional, short,
                            showDefault, strOption, value)

-- External imports: handling of input projects and command results
import Command.Result ( Result(..) )
import Data.Location  ( Location(..) )
import Data.Project   ( Project (..), readProject )

-- External imports: actions or commands supported
import           Command.Standalone (ErrorCode)
import qualified Command.Standalone

-- * Command

-- | Options to generate Copilot from specification.
data CommandOpts = CommandOpts
  { standaloneProject       :: Maybe String
  , standaloneTargetDir     :: FilePath
  , standaloneTemplateDir   :: Maybe FilePath
  , standaloneConditionExpr :: Maybe String
  , standaloneInputFiles    :: [FilePath]
  , standaloneFormat        :: String
  , standalonePropFormat    :: String
  , standaloneTypes         :: [String]
  , standaloneTarget        :: String
  , standalonePropVia       :: Maybe String
  , standaloneTemplateVars  :: Maybe String
  }

-- | Transform an input specification into a Copilot specification.
command :: CommandOpts -> IO (Result ErrorCode)
command c
    | Just p <- standaloneProject c
    = do optE <- commandProjectOptions p c
         case optE of
           Left msg  -> return $ Error cannotReadProject msg (LocationFile p)
           Right opt -> Command.Standalone.command opt

    | otherwise
    = Command.Standalone.command internalCommandOpts
  where
    internalCommandOpts :: Command.Standalone.CommandOptions
    internalCommandOpts = Command.Standalone.CommandOptions
      { Command.Standalone.commandConditionExpr = standaloneConditionExpr c
      , Command.Standalone.commandInputFiles    = standaloneInputFiles c
      , Command.Standalone.commandTargetDir     = standaloneTargetDir c
      , Command.Standalone.commandTemplateDir   = standaloneTemplateDir c
      , Command.Standalone.commandFormat        = standaloneFormat c
      , Command.Standalone.commandPropFormat    = standalonePropFormat c
      , Command.Standalone.commandTypeMapping   = types
      , Command.Standalone.commandFilename      = standaloneTarget c
      , Command.Standalone.commandPropVia       = standalonePropVia c
      , Command.Standalone.commandExtraVars     = standaloneTemplateVars c
      }

    types :: [(String, String)]
    types = typeMapping (standaloneTypes c)

-- | Produce command options based on project settings and user-provided
-- command options.
commandProjectOptions :: FilePath
                      -> CommandOpts
                      -> IO (Either String Command.Standalone.CommandOptions)
commandProjectOptions projectFile c = do
  projectE <- readProject projectFile
  return $ projectE <&> \project ->
    Command.Standalone.CommandOptions
      { Command.Standalone.commandConditionExpr = standaloneConditionExpr c

      , Command.Standalone.commandInputFiles = concat
          [ map (\(f, _, _) -> f) $ projectInputFiles project
          , standaloneInputFiles c
          ]

      , Command.Standalone.commandTargetDir =
          fromMaybe (standaloneTargetDir c) (projectTargetDir project)

      , Command.Standalone.commandTemplateDir =
          projectTemplateDir project <|> standaloneTemplateDir c

      , Command.Standalone.commandFormat =
          case projectInputFiles project of
            []            -> standaloneFormat c
            ((_, f, _):_) -> f

      , Command.Standalone.commandPropFormat =
          case projectInputFiles project of
            []            -> standalonePropFormat c
            ((_, _, f):_) -> f

      , Command.Standalone.commandPropVia =
          projectCommandPropVia project <|> standalonePropVia c

      , Command.Standalone.commandExtraVars =
          projectExtraJSONFile project <|> standaloneTemplateVars c

      , Command.Standalone.commandFilename = standaloneTarget c

      , Command.Standalone.commandTypeMapping = typeMapping (standaloneTypes c)

      }

-- | Parse a list of type associations, where two types are separated by ':'.
typeMapping :: [String] -> [(String, String)]
typeMapping = map splitTypeMapping
  where
    splitTypeMapping :: String -> (String, String)
    splitTypeMapping s = (h, safeTail t)
      where
        (h, t)      = span (/= ':') s
        safeTail xs = if null xs then xs else tail xs

-- * CLI

-- | Command description for CLI help.
commandDesc :: String
commandDesc = "Generate a standalone Copilot file from an input specification"

-- | Subparser for the @standalone@ command, used to generate a Copilot
-- specification from an input specification file.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> optional
        ( strOption
            (  long "project"
            <> metavar "FILENAME"
            <> help strStandaloneProjectArgDesc
            )
        )
  <*> strOption
        (  long "target-dir"
        <> metavar "DIR"
        <> showDefault
        <> value "copilot"
        <> help strStandaloneTargetDirDesc
        )
  <*> optional
        ( strOption
            (  long "template-dir"
            <> metavar "DIR"
            <> help strStandaloneTemplateDirArgDesc
            )
        )
  <*> optional
        ( strOption
          (  long "condition-expr"
          <> metavar "FILENAME"
          <> help strStandaloneConditionExprDesc
          )
        )
  <*> many
        ( strOption
          (  long "input-file"
          <> metavar "FILENAME"
          <> help strStandaloneInputFileDesc
          )
        )
  <*> strOption
        (  long "input-format"
        <> short 'f'
        <> metavar "FORMAT_NAME"
        <> help strStandaloneFormatDesc
        <> showDefault
        <> value "default"
        )
  <*> strOption
        (  long "prop-format"
        <> short 'p'
        <> metavar "FORMAT_NAME"
        <> help strStandalonePropFormatDesc
        <> showDefault
        <> value "smv"
        )
  <*> many (strOption
              (  long "map-type"
              <> short 'm'
              <> metavar "TYPE_NAME:TYPE_NAME"
              <> help strStandaloneMapTypeDesc
              )
           )
  <*> strOption
        (  long "target-file-name"
        <> metavar "FILENAME"
        <> help strStandaloneTargetDesc
        <> showDefault
        <> value "monitor"
        )
  <*> optional
        ( strOption
            (  long "parse-prop-via"
            <> metavar "COMMAND"
            <> help strStandalonePropViaDesc
            )
        )
  <*> optional
        ( strOption
            (  long "template-vars"
            <> metavar "FILENAME"
            <> help strStandaloneTemplateVarsArgDesc
            )
        )

-- | Project flag description.
strStandaloneProjectArgDesc :: String
strStandaloneProjectArgDesc = "Project file"

-- | Target dir flag description.
strStandaloneTargetDirDesc :: String
strStandaloneTargetDirDesc = "Target directory"

-- | Template dir flag description.
strStandaloneTemplateDirArgDesc :: String
strStandaloneTemplateDirArgDesc = "Directory holding standalone source template"

-- | Condition flag description.
strStandaloneConditionExprDesc :: String
strStandaloneConditionExprDesc =
  "Condition upon which the monitor will fire or notify"

-- | Input files flag description.
strStandaloneInputFileDesc :: String
strStandaloneInputFileDesc = "File(s) with properties or requirements"

-- | Format flag description.
strStandaloneFormatDesc :: String
strStandaloneFormatDesc = "Format of the input file"

-- | Property format flag description.
strStandalonePropFormatDesc :: String
strStandalonePropFormatDesc = "Format of temporal or boolean properties"

-- | Type mapping flag description.
strStandaloneMapTypeDesc :: String
strStandaloneMapTypeDesc = "Map a type to another type"

-- | Target file name flag description.
strStandaloneTargetDesc :: String
strStandaloneTargetDesc =
  "Filename prefix for monitoring files in target language"

-- | External command to pre-process individual properties.
strStandalonePropViaDesc :: String
strStandalonePropViaDesc = "Command to pre-process individual properties"

-- | Additional template variable file flag description.
strStandaloneTemplateVarsArgDesc :: String
strStandaloneTemplateVarsArgDesc =
  "JSON file containing additional variables to expand in template"

-- | Error code for when a project cannot be read.
cannotReadProject :: ErrorCode
cannotReadProject = 1
