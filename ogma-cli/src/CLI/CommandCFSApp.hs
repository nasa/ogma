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
import Options.Applicative ( Parser, help, long, metavar, optional, short,
                             showDefault, strOption, value )

-- External imports: command results
import Command.Result ( Result )

-- External imports: actions or commands supported
import           Command.CFSApp ( ErrorCode )
import qualified Command.CFSApp

-- * Command

-- | Options needed to generate the cFS application.
data CommandOpts = CommandOpts
  { cFSAppConditionExpr  :: Maybe String
  , cFSAppInputFile    :: Maybe String
  , cFSAppTarget       :: String
  , cFSAppTemplateDir  :: Maybe String
  , cFSAppVarNames     :: Maybe String
  , cFSAppVarDB        :: Maybe String
  , cFSAppHandlers     :: Maybe String
  , cFSAppFormat       :: String
  , cFSAppPropFormat   :: String
  , cFSAppPropVia      :: Maybe String
  , cFSAppTemplateVars :: Maybe String
  }

-- | Create <https://cfs.gsfc.nasa.gov/ NASA core Flight System> (cFS)
-- applications that subscribe to the communication bus and call Copilot when
-- new messages arrive.
--
-- This is just an uncurried version of "Command.CFSApp".
command :: CommandOpts -> IO (Result ErrorCode)
command c = Command.CFSApp.command options
  where
    options = Command.CFSApp.CommandOptions
                { Command.CFSApp.commandConditionExpr = cFSAppConditionExpr c
                , Command.CFSApp.commandInputFile   = cFSAppInputFile c
                , Command.CFSApp.commandTargetDir   = cFSAppTarget c
                , Command.CFSApp.commandTemplateDir = cFSAppTemplateDir c
                , Command.CFSApp.commandVariables   = cFSAppVarNames c
                , Command.CFSApp.commandVariableDB  = cFSAppVarDB c
                , Command.CFSApp.commandHandlers    = cFSAppHandlers c
                , Command.CFSApp.commandFormat      = cFSAppFormat c
                , Command.CFSApp.commandPropFormat  = cFSAppPropFormat c
                , Command.CFSApp.commandPropVia     = cFSAppPropVia c
                , Command.CFSApp.commandExtraVars   = cFSAppTemplateVars c
                }

-- * CLI

-- | cFS command description
commandDesc :: String
commandDesc = "Generate a complete cFS/Copilot application"

-- | Subparser for the @cfs@ command, used to generate a NASA Core Flight
-- System application connected to Copilot monitors.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> optional
        ( strOption
            (  long "condition-expr"
            <> metavar "EXPRESSION"
            <> help strCFSAppConditionExprArgDesc
            )
        )
  <*> optional
        ( strOption
            (  long "input-file"
            <> metavar "FILENAME"
            <> help strCFSAppFileNameArgDesc
            )
        )
  <*> strOption
        (  long "app-target-dir"
        <> metavar "DIR"
        <> showDefault
        <> value "copilot-cfs-demo"
        <> help strCFSAppDirArgDesc
        )
  <*> optional
        ( strOption
            (  long "app-template-dir"
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
        <> value "fcs"
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
  <*> optional
        ( strOption
            (  long "template-vars"
            <> metavar "FILENAME"
            <> help strCFSAppTemplateVarsArgDesc
            )
        )

-- | Argument target directory to cFS app generation command
strCFSAppDirArgDesc :: String
strCFSAppDirArgDesc = "Target directory"

-- | Argument template directory to cFS app generation command
strCFSAppTemplateDirArgDesc :: String
strCFSAppTemplateDirArgDesc =
  "Directory holding cFS application source template"

-- | Argument expression to CFS app generation command.
strCFSAppConditionExprArgDesc :: String
strCFSAppConditionExprArgDesc =
  "Expression used as guard or trigger condition"

-- | Argument input file to CFS app generation command
strCFSAppFileNameArgDesc :: String
strCFSAppFileNameArgDesc =
  "File containing input specification"


-- | Argument variable list to cFS app generation command
strCFSAppVarListArgDesc :: String
strCFSAppVarListArgDesc =
  "File containing list of cFS/ICAROUS variables to make accessible"

-- | Argument variable database to cFS app generation command
strCFSAppVarDBArgDesc :: String
strCFSAppVarDBArgDesc =
  "File containing a DB of known cFS/ICAROUS variables"

-- | Argument handler list to cFS app generation command
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
strCFSAppPropViaDesc =
  "Command to pre-process individual properties"

-- | Argument template variables to cFS app generation command
strCFSAppTemplateVarsArgDesc :: String
strCFSAppTemplateVarsArgDesc =
  "JSON file containing additional variables to expand in template"
