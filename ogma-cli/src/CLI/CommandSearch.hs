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
-- | CLI interface to the Search subcommand.
module CLI.CommandSearch
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
import           Data.Functor        ((<&>))
import qualified Data.Text.Lazy      as T
import qualified Data.Text.Lazy.IO   as T
import           Options.Applicative (Parser, help, long, many, metavar,
                                      optional, short, showDefault, strOption,
                                      value)
import           Text.Microstache    (compileMustacheText, renderMustache)

-- External imports: handling of input projects and command results
import Command.Result (Result (..))
import Data.Location  (Location (..))
import Data.Project   (Project (..), readProject)

-- External imports: actions or commands supported
import           Command.Search (ErrorCode)
import qualified Command.Search

-- * Command

-- | Options to generate an search from the input specification(s).
data CommandOpts = CommandOpts
  { searchProject    :: Maybe String
  , searchQuery      :: String
  , searchInputFiles :: [SearchFile]
  }

-- | Options associated to a specific input file.
data SearchFile = SearchFile
  { searchFilePath       :: FilePath
  , searchFileFormat     :: String
  , searchFilePropFormat :: String
  , searchFilePropVia    :: Maybe String
  }

-- | Print an search of the input specification(s).
command :: CommandOpts -> IO (Result ErrorCode)
command c
    | Just p <- searchProject c
    = do optE <- commandProjectOptions p c
         case optE of
           Left msg  -> return $ Error cannotReadProject msg (LocationFile p)
           Right opt -> do
             (mOutput, result) <- Command.Search.command opt
             case mOutput of
               Just output ->
                 case outputString of
                   Right template ->
                     T.putStr $ renderMustache template (toJSON output)
                   _              -> putStrLn "Error"
               _           -> putStrLn "Error"
             return result

     | otherwise
     = do (mOutput, result) <-
            Command.Search.command internalCommandOpts

          case mOutput of
            Just output ->
              case outputString of
                Right template ->
                  T.putStr $ renderMustache template (toJSON output)
                _              -> putStrLn "Error"
            _           -> putStrLn "Error"
          return result

  where

    internalCommandOpts :: Command.Search.CommandOptions
    internalCommandOpts = Command.Search.CommandOptions
      { Command.Search.commandInputFiles  = map fileInfo (searchInputFiles c)
      , Command.Search.commandSearchQuery = searchQuery c
      }

    fileInfo f = Command.Search.SearchFile
      { Command.Search.searchFilePath       = searchFilePath   f
      , Command.Search.searchFileFormat     = searchFileFormat f
      , Command.Search.searchFilePropFormat = searchFilePropFormat f
      , Command.Search.searchFilePropVia    = searchFilePropVia f
      }

    outputString =
      compileMustacheText "output" $ T.unlines
        [ "{{#searchResultRequirements}}"
        , "{{requirementInfoLocation}}: requirement "
          <> "\"{{requirementInfoName}}\" matches"
        , "{{/searchResultRequirements}}"
        , "{{#searchResultDiagrams}}"
        , "{{diagramInfoLocation}}: diagram file matches"
        , "{{/searchResultDiagrams}}"
        ]

-- | Produce command options based on project settings and user-provided
-- command options.
commandProjectOptions :: FilePath
                      -> CommandOpts
                      -> IO (Either String Command.Search.CommandOptions)
commandProjectOptions projectFile c = do
    projectE <- readProject projectFile
    return $ projectE <&> \project ->
      Command.Search.CommandOptions
        { Command.Search.commandInputFiles = concat
            [ map (convertProjectFile project) $ projectInputFiles project
            , map convertInputFile $ searchInputFiles c
            ]
        , Command.Search.commandSearchQuery = searchQuery c
        }

  where

    convertProjectFile project (fp, format, propFormat) =
      Command.Search.SearchFile
        { Command.Search.searchFilePath       = fp
        , Command.Search.searchFileFormat     = format
        , Command.Search.searchFilePropFormat = propFormat
        , Command.Search.searchFilePropVia    =
            projectCommandPropVia project
        }
    convertInputFile f = Command.Search.SearchFile
      { Command.Search.searchFilePath       = searchFilePath   f
      , Command.Search.searchFileFormat     = searchFileFormat f
      , Command.Search.searchFilePropFormat = searchFilePropFormat f
      , Command.Search.searchFilePropVia    = searchFilePropVia f
      }

-- * CLI

-- | Command description for CLI help.
commandDesc :: String
commandDesc = "List items that match search query"

-- | Subparser for the @search@ command, used to generate an search
-- of the input specifications.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> optional
        ( strOption
            (  long "project"
            <> metavar "FILENAME"
            <> help strSearchProjectArgDesc
            )
        )
  <*> strOption
        (  long "query"
        <> metavar "STRING"
        <> help strSearchQueryArgDesc
        )
  <*> many searchFileOptsParser

-- | Subparser for information on one input file to be used with the @search@
-- command.
searchFileOptsParser :: Parser SearchFile
searchFileOptsParser = SearchFile
  <$> strOption
        (  long "input-file"
        <> metavar "FILENAME"
        <> help strSearchInputFileDesc
        )
  <*> strOption
        (  long "input-format"
        <> short 'f'
        <> metavar "FORMAT_NAME"
        <> help strSearchFormatDesc
        <> showDefault
        <> value "default"
        )
  <*> strOption
        (  long "prop-format"
        <> short 'p'
        <> metavar "FORMAT_NAME"
        <> help strSearchPropFormatDesc
        <> showDefault
        <> value "smv"
        )
  <*> optional
        ( strOption
            (  long "parse-prop-via"
            <> metavar "COMMAND"
            <> help strSearchPropViaDesc
            )
        )

-- | Project flag description.
strSearchProjectArgDesc :: String
strSearchProjectArgDesc = "Project file"

-- | Query flag description.
strSearchQueryArgDesc :: String
strSearchQueryArgDesc = "Search string"

-- | Input file flag description.
strSearchInputFileDesc :: String
strSearchInputFileDesc = "File with properties or requirements"

-- | Format flag description.
strSearchFormatDesc :: String
strSearchFormatDesc = "Format of the input file"

-- | Property format flag description.
strSearchPropFormatDesc :: String
strSearchPropFormatDesc = "Format of temporal or boolean properties"

-- | External command to pre-process individual properties.
strSearchPropViaDesc :: String
strSearchPropViaDesc = "Command to pre-process individual properties"

-- | Error code for when a project cannot be read.
cannotReadProject :: ErrorCode
cannotReadProject = 1
