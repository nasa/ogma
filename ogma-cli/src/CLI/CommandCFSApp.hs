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
import Options.Applicative ( Parser, help, long, metavar, optional, showDefault,
                             strOption, value )

-- External imports: command results
import Command.Result ( Result )

-- External imports: actions or commands supported
import Command.CFSApp ( ErrorCode, cFSApp )

-- * Command

-- | Options needed to generate the cFS application.
data CommandOpts = CommandOpts
  { cFSAppTarget   :: String
  , cFSAppVarNames :: String
  , cFSAppVarDB    :: Maybe String
  }

-- | Create <https://cfs.gsfc.nasa.gov/ NASA core Flight System> (cFS)
-- applications that subscribe to the communication bus and call Copilot when
-- new messages arrive.
--
-- This is just an uncurried version of "Command.CFSApp".
command :: CommandOpts -> IO (Result ErrorCode)
command c =
  cFSApp (cFSAppTarget c) (cFSAppVarNames c) (cFSAppVarDB c)

-- * CLI

-- | cFS command description
commandDesc :: String
commandDesc = "Generate a complete cFS/Copilot application"

-- | Subparser for the @cfs@ command, used to generate a NASA Core Flight
-- System application connected to Copilot monitors.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> strOption
        (  long "app-target-dir"
        <> metavar "DIR"
        <> showDefault
        <> value "copilot-cfs-demo"
        <> help strCFSAppDirArgDesc
        )
  <*> strOption
        (  long "variable-file"
        <> metavar "FILENAME"
        <> showDefault
        <> value "variables"
        <> help strCFSAppVarListArgDesc
        )
  <*> optional
        ( strOption
            (  long "variable-db"
            <> metavar "FILENAME"
            <> help strCFSAppVarDBArgDesc
            )
        )

-- | Argument target directory to cFS app generation command
strCFSAppDirArgDesc :: String
strCFSAppDirArgDesc = "Target directory"

-- | Argument variable list to cFS app generation command
strCFSAppVarListArgDesc :: String
strCFSAppVarListArgDesc =
  "File containing list of cFS/ICAROUS variables to make accessible"

-- | Argument variable database to cFS app generation command
strCFSAppVarDBArgDesc :: String
strCFSAppVarDBArgDesc =
  "File containing a DB of known cFS/ICAROUS variables"
