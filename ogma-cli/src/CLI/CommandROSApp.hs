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
import Options.Applicative ( Parser, help, long, metavar, optional, short,
                             showDefault, strOption, value )

-- External imports: command results
import Command.Result ( Result )

-- External imports: actions or commands supported
import           Command.ROSApp (ErrorCode)
import qualified Command.ROSApp

-- * Command

-- | Options needed to generate the ROS application.
data CommandOpts = CommandOpts
  { rosAppInputFile    :: Maybe String
  , rosAppTarget       :: String
  , rosAppTemplateDir  :: Maybe String
  , rosAppVarNames     :: Maybe String
  , rosAppVarDB        :: Maybe String
  , rosAppHandlers     :: Maybe String
  , rosAppFormat       :: String
  , rosAppPropFormat   :: String
  , rosAppPropVia      :: Maybe String
  , rosAppTemplateVars :: Maybe String
  }

-- | Create <https://www.ros.org/ Robot Operating System> (ROS) applications
-- that subscribe to obtain necessary data from topics and call Copilot when
-- new data arrives.
--
-- This is just a wrapper around "Command.ROSApp".
command :: CommandOpts -> IO (Result ErrorCode)
command c = Command.ROSApp.command options
  where
    options = Command.ROSApp.CommandOptions
                { Command.ROSApp.commandInputFile   = rosAppInputFile c
                , Command.ROSApp.commandTargetDir   = rosAppTarget c
                , Command.ROSApp.commandTemplateDir = rosAppTemplateDir c
                , Command.ROSApp.commandVariables   = rosAppVarNames c
                , Command.ROSApp.commandVariableDB  = rosAppVarDB c
                , Command.ROSApp.commandHandlers    = rosAppHandlers c
                , Command.ROSApp.commandFormat      = rosAppFormat c
                , Command.ROSApp.commandPropFormat  = rosAppPropFormat c
                , Command.ROSApp.commandPropVia     = rosAppPropVia c
                , Command.ROSApp.commandExtraVars   = rosAppTemplateVars c
                }

-- * CLI

-- | ROS command description
commandDesc :: String
commandDesc = "Generate a ROS 2 monitoring package"

-- | Subparser for the @ros@ command, used to generate a Robot Operating System
-- application connected to Copilot monitors.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> optional
        ( strOption
            (  long "input-file"
            <> metavar "FILENAME"
            <> help strROSAppFileNameArgDesc
            )
        )
  <*> strOption
        (  long "app-target-dir"
        <> metavar "DIR"
        <> showDefault
        <> value "ros"
        <> help strROSAppDirArgDesc
        )
  <*> optional
        ( strOption
            (  long "app-template-dir"
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
        <> value "fcs"
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

-- | Argument target directory to ROS app generation command
strROSAppDirArgDesc :: String
strROSAppDirArgDesc = "Target directory"

-- | Argument template directory to ROS app generation command
strROSAppTemplateDirArgDesc :: String
strROSAppTemplateDirArgDesc =
  "Directory holding ROS application source template"

-- | Argument input file to ROS app generation command
strROSAppFileNameArgDesc :: String
strROSAppFileNameArgDesc =
  "File containing input specification"

-- | Argument variable list to ROS app generation command
strROSAppVarListArgDesc :: String
strROSAppVarListArgDesc =
  "File containing list of ROS variables to make accessible"

-- | Argument variable database to ROS app generation command
strROSAppVarDBArgDesc :: String
strROSAppVarDBArgDesc =
  "File containing a DB of known ROS variables"

-- | Argument handler list to ROS app generation command
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
strROSAppPropViaDesc =
  "Command to pre-process individual properties"

-- | Additional template variable file flag description.
strROSAppTemplateVarsArgDesc :: String
strROSAppTemplateVarsArgDesc =
  "JSON file containing additional variables to expand in template"
