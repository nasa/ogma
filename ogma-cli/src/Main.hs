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
-- | Ogma: Tool to interoperate between <https://cfs.gsfc.nasa.gov/ Copilot>
-- and other languages.
--
-- Ogma is a tool to facilitate integration of safe runtime monitors into other
-- systems. It takes information from a system created in a language (e.g.,
-- CoCoSpec ) and produces specifications for the runtime verification
-- framework <https://cfs.gsfc.nasa.gov/ Copilot>. Currently, features
-- supported are:
--
-- * Translation of ptLTL and Cocospec properties defined in a
-- <https://github.com/NASA-SW-VnV/fret FRET> file into corresponding
-- expressions in Copilot.
--
-- * Translation of C headers declaring structs into the corresponding Copilot
-- Struct definitions.
--
-- * Translation of C headers declaring structs into CFS message handlers that
-- copy data in global variables.
--
-- * Generate NASA core Flight System (cFS) applications for runtime monitoring
-- using Copilot.
--
-- * Generate Robot Operating System (ROS) applications for runtime monitoring
-- using Copilot.
--
-- * Generate F' (FPrime) components for runtime monitoring using Copilot.
--
-- More information can be obtained by calling ogma with the argument @--help@.
module Main
    ( main )
  where

-- External imports
import Options.Applicative (ParserInfo, execParser, fullDesc, header, helper,
                            info, progDesc, (<**>))

-- Internal imports: CLI parsing, handling, and processing of results.
import CLI.CommandTop ( CommandOpts, command, commandDesc, commandOptsParser )
import CLI.Result     ( processResult )

-- | Ogma: Helper tool to interoperate between Copilot and other languages.
main :: IO ()
main = execParser fullCLIOpts >>= command >>= processResult

-- | Full program options.
fullCLIOpts :: ParserInfo CommandOpts
fullCLIOpts = info (commandOptsParser <**> helper)
  (  fullDesc
  <> progDesc commandDesc
  <> header strProgramSummary
  )

-- | Short program description
strProgramSummary :: String
strProgramSummary =
  "ogma - an anything-to-Copilot application generator"
