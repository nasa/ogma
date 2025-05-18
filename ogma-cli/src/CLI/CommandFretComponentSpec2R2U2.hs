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
-- | CLI interface to the FRET CS 2 R2U2 subcommand
module CLI.CommandFretComponentSpec2R2U2
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
import Command.FRETComponentSpec2R2U2 ( ErrorCode,
                                        FRETComponentSpec2R2U2Options(..),
                                        fretComponentSpec2R2U2 )

-- * Command

-- | Options to generate R2U2 from FRET Component Specifications.
data CommandOpts = CommandOpts
  { fretComponentSpecFileName :: FilePath
  , fretComponentSpecCoCoSpec :: Bool
  , fretComponentSpecIntType  :: String
  , fretComponentSpecRealType :: String
  }

-- | Transform a FRET component specification into a R2U2 specification.
--
-- This is just an uncurried version of "Command.FRETComponentSpec2R2U2".
command :: CommandOpts -> IO (Result ErrorCode)
command c =
    fretComponentSpec2R2U2
      (fretComponentSpecFileName c)
      internalCommandOpts

  where

    internalCommandOpts :: FRETComponentSpec2R2U2Options
    internalCommandOpts = FRETComponentSpec2R2U2Options
      { fretCS2R2U2UseCoCoSpec = fretComponentSpecCoCoSpec c
      , fretCS2R2U2IntType     = fretComponentSpecIntType  c
      , fretCS2R2U2RealType    = fretComponentSpecRealType c
      }

-- * CLI

-- | Command description for CLI help.
commandDesc :: String
commandDesc =
  "Generate an R2U2 file from a FRET Component Specification"

-- | Subparser for the @fret-component-spec@ command, used to generate a
-- R2U2 specification from a FRET component specification file.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> strOption
        (  long "fret-file-name"
        <> metavar "FILENAME"
        <> help strFretArgDesc
        )
  <*> switch
        (  long "cocospec"
        <> help strFretCoCoDesc
        )
  <*> strOption
        (  long "map-int-to"
        <> short 'i'
        <> metavar "TYPE_NAME"
        <> help strFretIntTypeDesc
        <> showDefault
        <> value "int"
        )
  <*> strOption
        (  long "map-real-to"
        <> short 'r'
        <> metavar "TYPE_NAME"
        <> help strFretRealTypeDesc
        <> showDefault
        <> value "float"
        )

-- | Argument FRET command description
strFretArgDesc :: String
strFretArgDesc = "FRET file with requirements."

-- | CoCoSpec flag description
strFretCoCoDesc :: String
strFretCoCoDesc = "Use CoCoSpec variant of TL properties"

-- | Int type mapping flag description.
strFretIntTypeDesc :: String
strFretIntTypeDesc = "Map integer variables to the given type"

-- | Real type mapping flag description.
strFretRealTypeDesc :: String
strFretRealTypeDesc = "Map real variables to the given type"
