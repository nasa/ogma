-- Copyright 2020 United States Government as represented by the Administrator
-- of the National Aeronautics and Space Administration. All Rights Reserved.
--
-- Disclaimers
--
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY
-- OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT
-- LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO
-- SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
-- PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE
-- SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF
-- PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN
-- ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR
-- RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR
-- ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE. FURTHER,
-- GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING
-- THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES
-- IT "AS IS."
--
-- Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST
-- THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS
-- ANY PRIOR RECIPIENT. IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN
-- ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE,
-- INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S
-- USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE
-- UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY
-- PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW. RECIPIENT'S SOLE REMEDY
-- FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS
-- AGREEMENT.
--
-- | CLI interface to the Standalone subcommand
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
import Options.Applicative (Parser, help, long, many, metavar, optional, short,
                            showDefault, strOption, switch, value)

-- External imports: command results
import Command.Result ( Result(..) )
import Data.Location  ( Location(..) )

-- External imports: actions or commands supported
import           Command.Standalone (ErrorCode)
import qualified Command.Standalone

-- * Command

-- | Options to generate Copilot from specification.
data CommandOpts = CommandOpts
  { standaloneTargetDir   :: FilePath
  , standaloneTemplateDir :: Maybe FilePath
  , standaloneFileName    :: FilePath
  , standaloneFormat      :: String
  , standalonePropFormat  :: String
  , standaloneTypes       :: [String]
  , standaloneTarget      :: String
  , standalonePropVia     :: Maybe String
  }

-- | Transform an input specification into a Copilot specification.
command :: CommandOpts -> IO (Result ErrorCode)
command c =
    Command.Standalone.command internalCommandOpts
  where
    internalCommandOpts :: Command.Standalone.CommandOptions
    internalCommandOpts = Command.Standalone.CommandOptions
      { Command.Standalone.commandInputFile   = standaloneFileName c
      , Command.Standalone.commandTargetDir   = standaloneTargetDir c
      , Command.Standalone.commandTemplateDir = standaloneTemplateDir c
      , Command.Standalone.commandFormat      = standaloneFormat c
      , Command.Standalone.commandPropFormat  = standalonePropFormat c
      , Command.Standalone.commandTypeMapping = types
      , Command.Standalone.commandFilename    = standaloneTarget c
      , Command.Standalone.commandPropVia     = standalonePropVia c
      }

    types :: [(String, String)]
    types = map splitTypeMapping (standaloneTypes c)

    splitTypeMapping :: String -> (String, String)
    splitTypeMapping s = (h, safeTail t)
      where
        (h, t)      = span (/= ':') s
        safeTail xs = if null xs then xs else tail xs

-- * CLI

-- | Command description for CLI help.
commandDesc :: String
commandDesc =
  "Generate a standalone Copilot file from an input specification"

-- | Subparser for the @standalone@ command, used to generate a Copilot
-- specification from an input specification file.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> strOption
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
  <*> strOption
        (  long "file-name"
        <> metavar "FILENAME"
        <> help strStandaloneFilenameDesc
        )
  <*> strOption
        (  long "input-format"
        <> short 'f'
        <> metavar "FORMAT_NAME"
        <> help strStandaloneFormatDesc
        <> showDefault
        <> value "fcs"
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

-- | Target dir flag description.
strStandaloneTargetDirDesc :: String
strStandaloneTargetDirDesc = "Target directory"

-- | Template dir flag description.
strStandaloneTemplateDirArgDesc :: String
strStandaloneTemplateDirArgDesc = "Directory holding standalone source template"

-- | Filename flag description.
strStandaloneFilenameDesc :: String
strStandaloneFilenameDesc = "File with properties or requirements"

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
strStandalonePropViaDesc =
  "Command to pre-process individual properties"
