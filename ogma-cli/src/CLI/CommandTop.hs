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
-- | CLI parsing, handling, and execution.
--
-- The full module CLI is just a sum of all the subcommands with their
-- respective CLI options.
--
-- This module defines:
--
-- - A 'CommandOpts' type that represents all options to the program,
-- the subcommand executed, and any options for that subcommand.
--
-- - A 'commandOptsParser' parser for the subcommands and their arguments,
-- delegating the argument parsing to each subcommand's own CLI parser.
--
-- - A 'command' function that dispatcher processing of the execution to the
-- appropriate subcommand.
--
-- - An 'ErrorCode' type that represents reasons why the program may have
-- failed.
--
-- Each of the subcommands is defined using the same standard interface.

-- This module is merely a sum of all subcommands:
--
-- - The type 'CommandOpts' is the sum of all respective subcommand options
-- (whose types all share the same name but have different representations).
--
-- - The parser is a sum of all subparsers, each attached to a specific
-- command name defined in this module.
--
-- - The command function is merely a dispatcher that detects the subcommand
-- that must be executed, extracts the input, runs it, and translates the
-- subcommands result into a top-level command result.
--
-- - The error code is a plain representation that collapses the local
-- subcommand error codes into a few possible values.

module CLI.CommandTop
    (
      -- * Direct command access
      CommandOpts
    , command
    , ErrorCode

      -- * CLI
    , commandDesc
    , commandOptsParser
    )
  where

-- External imports
import           Options.Applicative ( CommandFields, Mod, Parser, helper, info,
                                       progDesc, subparser, (<**>) )
import qualified Options.Applicative as OptParse

-- External imports: command results
import Command.Result ( Result )

-- Internal imports: subcommands
import qualified CLI.CommandCFSApp
import qualified CLI.CommandCStructs2Copilot
import qualified CLI.CommandCStructs2MsgHandlers
import qualified CLI.CommandDiagram
import qualified CLI.CommandFPrimeApp
import qualified CLI.CommandReport
import qualified CLI.CommandROSApp
import qualified CLI.CommandStandalone

-- * Command

-- | Commands supported by Ogma and their arguments.

-- All subcommands use the same interface, so they all use the type name
-- @CommandOpts@ to capture their respective arguments. These types are
-- different for each subcommand.
data CommandOpts =
    CommandOptsCFSApp                    CLI.CommandCFSApp.CommandOpts
  | CommandOptsCStructs2Copilot          CLI.CommandCStructs2Copilot.CommandOpts
  | CommandOptsCStructs2MsgHandlers      CLI.CommandCStructs2MsgHandlers.CommandOpts
  | CommandOptsDiagram                   CLI.CommandDiagram.CommandOpts
  | CommandOptsFPrimeApp                 CLI.CommandFPrimeApp.CommandOpts
  | CommandOptsROSApp                    CLI.CommandROSApp.CommandOpts
  | CommandOptsStandalone                CLI.CommandStandalone.CommandOpts
  | CommandOptsReport                    CLI.CommandReport.CommandOpts

-- * CLI

-- | Command description for CLI help.
commandDesc :: String
commandDesc =
  "Generate complete or partial Copilot applications from multiple languages"

-- | Subparser for multiple subcommands.
commandOptsParser :: Parser CommandOpts
commandOptsParser = subparser
  (  subcommandCStructs
  <> subcommandMsgHandlers
  <> subcommandCFSApp
  <> subcommandFPrimeApp
  <> subcommandROSApp
  <> subcommandStandalone
  <> subcommandDiagram
  <> subcommandReport
  )

-- | Modifier for the CStruct to Copilot Struct generation subcommand, linking
-- the subcommand options and description to the command @structs@ at top
-- level.
subcommandCStructs :: Mod CommandFields CommandOpts
subcommandCStructs =
  subcommand
    "structs"
    (CommandOptsCStructs2Copilot
       <$> CLI.CommandCStructs2Copilot.commandOptsParser)
    CLI.CommandCStructs2Copilot.commandDesc

-- | Modifier for the msg handler generation subcommand, linking the subcommand
-- options and description to the command @handlers@ at top level.
subcommandMsgHandlers :: Mod CommandFields CommandOpts
subcommandMsgHandlers =
  subcommand
    "handlers"
    (CommandOptsCStructs2MsgHandlers
       <$> CLI.CommandCStructs2MsgHandlers.commandOptsParser)
    CLI.CommandCStructs2MsgHandlers.commandDesc

-- | Modifier for the CFS app expansion subcommand, linking the subcommand
-- options and description to the command @cfs@ at top level.
subcommandCFSApp :: Mod CommandFields CommandOpts
subcommandCFSApp =
  subcommand
    "cfs"
    (CommandOptsCFSApp <$> CLI.CommandCFSApp.commandOptsParser)
    CLI.CommandCFSApp.commandDesc

-- | Modifier for the ROS app expansion subcommand, linking the subcommand
-- options and description to the command @ros@ at top level.
subcommandROSApp :: Mod CommandFields CommandOpts
subcommandROSApp =
  subcommand
    "ros"
    (CommandOptsROSApp <$> CLI.CommandROSApp.commandOptsParser)
    CLI.CommandROSApp.commandDesc

-- | Modifier for the FPrime app expansion subcommand, linking the subcommand
-- options and description to the command @fprime@ at top level.
subcommandFPrimeApp :: Mod CommandFields CommandOpts
subcommandFPrimeApp =
  subcommand
    "fprime"
    (CommandOptsFPrimeApp <$> CLI.CommandFPrimeApp.commandOptsParser)
    CLI.CommandFPrimeApp.commandDesc

-- | Modifier for the standalone subcommand, linking the subcommand options and
-- description to the command @standalone@ at top level.
subcommandStandalone :: Mod CommandFields CommandOpts
subcommandStandalone =
  subcommand
    "standalone"
    (CommandOptsStandalone <$> CLI.CommandStandalone.commandOptsParser)
    CLI.CommandStandalone.commandDesc

-- | Modifier for the diagram subcommand, linking the subcommand options and
-- description to the command @diagram@ at top level.
subcommandDiagram :: Mod CommandFields CommandOpts
subcommandDiagram =
  subcommand
    "diagram"
    (CommandOptsDiagram <$> CLI.CommandDiagram.commandOptsParser)
    CLI.CommandDiagram.commandDesc

-- | Modifier for the report subcommand, linking the subcommand options and
-- description to the command @report@ at top level.
subcommandReport :: Mod CommandFields CommandOpts
subcommandReport =
  subcommand
    "report"
    (CommandOptsReport <$> CLI.CommandReport.commandOptsParser)
    CLI.CommandReport.commandDesc

-- * Command dispatcher

-- | Command dispatcher that obtains the parameters from the command line and
-- passes them as arguments to the actual function that will process them,
-- transforming the local result into a global program result.

-- This function is implemented as a combination of three processes or
-- functions: one that adapts the input to the inner function (down), the
-- actual function implementing the command, and a translation of the local
-- result into a global result that can be reported to users (up). In this
-- case, the commands are all using the same (and compatible) exit codes, but
-- they might not do so. This is captured by the fmap of the function id on
-- each processing command.
--
-- The function that adapts the inputs simply passes the arguments obtained
-- from the command line one by one. Composition with this function uncurries
-- the next function to work over the product as defined by the record that
-- accompanies each command definition in the type OgmaCLICommand.
--
-- Neither this nor the internal commands not know, and need to know, that they
-- run in CLI.
command :: CommandOpts -> IO (Result ErrorCode)
command (CommandOptsCFSApp c) =
  id <$> CLI.CommandCFSApp.command c
command (CommandOptsCStructs2Copilot c) =
  id <$> CLI.CommandCStructs2Copilot.command c
command (CommandOptsCStructs2MsgHandlers c) =
  id <$> CLI.CommandCStructs2MsgHandlers.command c
command (CommandOptsFPrimeApp c) =
  id <$> CLI.CommandFPrimeApp.command c
command (CommandOptsROSApp c) =
  id <$> CLI.CommandROSApp.command c
command (CommandOptsStandalone c) =
  id <$> CLI.CommandStandalone.command c
command (CommandOptsDiagram c) =
  id <$> CLI.CommandDiagram.command c
command (CommandOptsReport c) =
  id <$> CLI.CommandReport.command c

-- We indicate to HLint that the use of (id <$>) above should not trigger a
-- warning. Conceptually, there is a transformation taking place, but no change
-- is required because the types used by the internal functions and the
-- top-level command to represent results are the same, and the error codes are
-- non-overlapping.
{-# ANN command "HLint: ignore Functor law" #-}

-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error code used is 1 for user error, 2 for internal bug.
type ErrorCode = Int

-- * Auxiliary defs

-- | Build a subcommand modifier from a command name, option parser and command
-- description.
subcommand :: String
           -> Parser CommandOpts
           -> String
           -> Mod CommandFields CommandOpts
subcommand entry parser desc =
  OptParse.command entry (info (parser <**> helper) (progDesc desc))
