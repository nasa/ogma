{-# LANGUAGE OverloadedStrings #-}
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
-- | CLI interface to the Report subcommand.
module CLI.CommandReport
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
import           Command.Report (ErrorCode)
import qualified Command.Report

-- * Command

-- | Options to generate Copilot from specification.
data CommandOpts = CommandOpts
  { reportFileName    :: FilePath
  , reportFormat      :: String
  , reportPropFormat  :: String
  , reportPropVia     :: Maybe String
  }

-- | Print an report of an input specification.
command :: CommandOpts -> IO (Result ErrorCode)
command c = do
    (mOutput, result) <-
      Command.Report.command (reportFileName c) internalCommandOpts

    case (mOutput, outputString) of
      (Just output, Right template) ->
         T.putStr $ renderMustache template (toJSON output)
      _ -> putStrLn "Error"
    return result

  where
    internalCommandOpts :: Command.Report.CommandOptions
    internalCommandOpts = Command.Report.CommandOptions
      { Command.Report.commandFormat      = reportFormat c
      , Command.Report.commandPropFormat  = reportPropFormat c
      , Command.Report.commandPropVia     = reportPropVia c
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
commandDesc =
  "Generate a report Copilot file from an input specification"

-- | Subparser for the @report@ command, used to generate a Copilot
-- specification from an input specification file.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> strOption
        (  long "file-name"
        <> metavar "FILENAME"
        <> help strReportFilenameDesc
        )
  <*> strOption
        (  long "input-format"
        <> short 'f'
        <> metavar "FORMAT_NAME"
        <> help strReportFormatDesc
        <> showDefault
        <> value "fcs"
        )
  <*> strOption
        (  long "prop-format"
        <> short 'p'
        <> metavar "FORMAT_NAME"
        <> help strReportPropFormatDesc
        <> showDefault
        <> value "smv"
        )
  <*> optional
        ( strOption
            (  long "parse-prop-via"
            <> metavar "COMMAND"
            <> help strReportPropViaDesc
            )
        )

-- | Filename flag description.
strReportFilenameDesc :: String
strReportFilenameDesc = "File with properties or requirements"

-- | Format flag description.
strReportFormatDesc :: String
strReportFormatDesc = "Format of the input file"

-- | Property format flag description.
strReportPropFormatDesc :: String
strReportPropFormatDesc = "Format of temporal or boolean properties"

-- | External command to pre-process individual properties.
strReportPropViaDesc :: String
strReportPropViaDesc =
  "Command to pre-process individual properties"
