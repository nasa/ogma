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
-- | CLI interface to the XML 2 Copilot subcommand
module CLI.CommandXMLSpec2Copilot
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
import Options.Applicative (Parser, help, long, metavar, short, showDefault,
                            strOption, switch, value)

-- External imports: command results
import Command.Result ( Result )

-- External imports: actions or commands supported
import Command.XMLSpec2Copilot (ErrorCode, XMLSpec2CopilotOptions (..),
                                xmlSpec2Copilot)

-- * Command

-- | Options to generate Copilot from XML Specifications.
data CommandOpts = CommandOpts
  { xmlSpecFileName :: FilePath
  , xmlSpecCoCoSpec :: Bool
  , xmlSpecIntType  :: String
  , xmlSpecRealType :: String
  , xmlSpecTarget   :: String
  }

-- | Transform an XML into a Copilot specification.
--
-- This is just an uncurried version of "Command.XMLSpec2Copilot".
command :: CommandOpts -> IO (Result ErrorCode)
command c =
    xmlSpec2Copilot
      (xmlSpecFileName c)
      internalCommandOpts

  where

    internalCommandOpts :: XMLSpec2CopilotOptions
    internalCommandOpts = XMLSpec2CopilotOptions
      { xmlSpec2CopilotUseCoCoSpec = xmlSpecCoCoSpec c
      , xmlSpec2CopilotIntType     = xmlSpecIntType  c
      , xmlSpec2CopilotRealType    = xmlSpecRealType c
      , xmlSpec2CopilotFilename    = xmlSpecTarget   c
      }

-- * CLI

-- | Command description for CLI help.
commandDesc :: String
commandDesc =
  "Generate a Copilot file from an XML file"

-- | Subparser for the @xml-spec@ command, used to generate a
-- Copilot specification from an XML file.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> strOption
        (  long "xml-file-name"
        <> metavar "FILENAME"
        <> help strXMLArgDesc
        )
  <*> switch
        (  long "cocospec"
        <> help strXMLCoCoDesc
        )
  <*> strOption
        (  long "map-int-to"
        <> short 'i'
        <> metavar "TYPE_NAME"
        <> help strXMLIntTypeDesc
        <> showDefault
        <> value "Int64"
        )
  <*> strOption
        (  long "map-real-to"
        <> short 'r'
        <> metavar "TYPE_NAME"
        <> help strXMLRealTypeDesc
        <> showDefault
        <> value "Float"
        )
  <*> strOption
        (  long "target-file-name"
        <> metavar "FILENAME"
        <> help strXMLTargetDesc
        <> showDefault
        <> value "monitor"
        )

-- | Argument XML command description
strXMLArgDesc :: String
strXMLArgDesc = "XML file with properties or requirements."

-- | CoCoSpec flag description
strXMLCoCoDesc :: String
strXMLCoCoDesc = "Interpret properties as CoCoSpec boolean expressions"

-- | Int type mapping flag description.
strXMLIntTypeDesc :: String
strXMLIntTypeDesc = "Map integer variables to the given type"

-- | Real type mapping flag description.
strXMLRealTypeDesc :: String
strXMLRealTypeDesc = "Map real variables to the given type"

-- | Target file name flag description.
strXMLTargetDesc :: String
strXMLTargetDesc = "Filename prefix for monitoring files in target language"
