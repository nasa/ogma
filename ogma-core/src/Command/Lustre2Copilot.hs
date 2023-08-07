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
-- | Transform a Lustre file into a Copilot specification.
module Command.Lustre2Copilot
    ( lustre2Copilot
    , ErrorCode
    )
  where

-- External imports
import System.Exit ( ExitCode (ExitFailure), exitWith )
import System.IO   ( hPutStrLn, stderr )

-- Internal imports: auxiliary
import Data.String.Extra as S ( safeReadFile )

-- Internal imports: auxiliary
import Command.Result ( Result (..) )
import Data.Location  ( Location (..) )

-- Internal imports: Lustre parsing and AST
import qualified Language.Lustre.AbsLustre as L ( Program )
import qualified Language.Lustre.ParLustre as L ( myLexer, pProgram )

-- Internal imports: transformation of Lustre to Copilot
import qualified Language.Trans.Lustre2Copilot as L ( lustre2Copilot )

-- | Print the contents of a Copilot module that implements monitors for
-- the units in a Lustre file.
--
-- PRE: The file given is readable, contains a valid Lustre file., none of the
-- variables used in Lustre clash with any identifiers that exist in Copilot.
-- All identifiers used are valid C99 identifiers.
lustre2Copilot :: FilePath
               -> IO (Result ErrorCode)
lustre2Copilot fp = do

    -- All of the following operations use Either to return error messages. The
    -- use of the monadic bind to pass arguments from one function to the next
    -- will cause the program to stop at the earliest error.
    --
    -- The guard (checking whether the result is Left or Right) must happen
    -- before the first side effects (output file creation), or the file may be
    -- (over) written even when processing fails.
    lustre <- parseLustreProgram fp

    case L.lustre2Copilot =<< lustre of
      Right spec  -> putStrLn spec >> return Success
      Left msg    -> return $ Error ecLustreError msg (LocationFile fp)

  where

    -- | Parse a Lustre file.
    --
    -- Returns a 'Left' with an error message if the file does not have the
    -- correct format.
    --
    -- Throws an exception if the file cannot be read.
    parseLustreProgram :: FilePath -> IO (Either String L.Program)
    parseLustreProgram fp = do
      content <- S.safeReadFile fp
      return $ L.pProgram . L.myLexer =<< content

-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error code used is 1 for user error.
type ErrorCode = Int

-- | Error: the Lustre file cannot be read due to the file being unreadable or
-- the format being incorrect.
ecLustreError :: ErrorCode
ecLustreError = 1
