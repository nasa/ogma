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
import qualified CLI.CommandCStructs2Copilot

-- * Command

-- | Commands supported by Ogma and their arguments.

-- All subcommands use the same interface, so they all use the type name
-- @CommandOpts@ to capture their respective arguments. These types are
-- different for each subcommand.
data CommandOpts =
  CommandOptsCStructs2Copilot CLI.CommandCStructs2Copilot.CommandOpts

-- * CLI

-- | Command description for CLI help.
commandDesc :: String
commandDesc =
  "Generate complete or partial Copilot applications from multiple languages"

-- | Subparser for multiple subcommands.
commandOptsParser :: Parser CommandOpts
commandOptsParser = subparser
  subcommandCStructs

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
command (CommandOptsCStructs2Copilot c) =
  id <$> CLI.CommandCStructs2Copilot.command c

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
