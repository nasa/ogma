-- Copyright 2022 United States Government as represented by the Administrator
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
import Options.Applicative ( Parser, help, long, metavar, optional, showDefault,
                             strOption, value )

-- External imports: command results
import Command.Result ( Result )

-- External imports: actions or commands supported
import Command.FPrimeApp ( ErrorCode, fprimeApp )

-- * Command

-- | Options needed to generate the FPrime component.
data CommandOpts = CommandOpts
  { fprimeAppTarget      :: String
  , fprimeAppTemplateDir :: Maybe String
  , fprimeAppFRETFile    :: Maybe String
  , fprimeAppVarNames    :: Maybe String
  , fprimeAppVarDB       :: Maybe String
  , fprimeAppHandlers    :: Maybe String
  }

-- | Create <https://github.com/nasa/fprime FPrime> component that subscribe
-- to obtain necessary data from the bus and call Copilot when new data
-- arrives.
--
-- This is just an uncurried version of "Command.fprimeApp".
command :: CommandOpts -> IO (Result ErrorCode)
command c =
  fprimeApp
    (fprimeAppTarget c)
    (fprimeAppTemplateDir c)
    (fprimeAppFRETFile c)
    (fprimeAppVarNames c)
    (fprimeAppVarDB c)
    (fprimeAppHandlers c)

-- * CLI

-- | FPrime command description
commandDesc :: String
commandDesc = "Generate a complete F' monitoring component"

-- | Subparser for the @fprime@ command, used to generate an FPrime component
-- connected to Copilot monitors.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> strOption
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
            (  long "fret-file-name"
            <> metavar "FILENAME"
            <> help strFPrimeAppFRETFileNameArgDesc
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

-- | Argument target directory to FPrime component generation command
strFPrimeAppDirArgDesc :: String
strFPrimeAppDirArgDesc = "Target directory"

-- | Argument template directory to FPrime component generation command
strFPrimeAppTemplateDirArgDesc :: String
strFPrimeAppTemplateDirArgDesc =
  "Directory holding F' component source template"

-- | Argument FRET CS to FPrime component generation command
strFPrimeAppFRETFileNameArgDesc :: String
strFPrimeAppFRETFileNameArgDesc =
  "File containing FRET Component Specification"

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
