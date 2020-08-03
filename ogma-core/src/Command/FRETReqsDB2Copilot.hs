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
-- | Transform a FRET requirements database containing a temporal logic
-- specification into a Copilot specification.
--
-- This module makes use of
-- "Language.Trans.FRETReqsDB2Copilot", which does most of the work.
module Command.FRETReqsDB2Copilot
    ( fretReqsDB2Copilot
    , ErrorCode
    )
  where

-- External imports
import Data.Aeson  ( eitherDecode )

-- External imports: auxiliary
import Data.ByteString.Extra as B ( safeReadFile )
import Data.List.Extra       ( headEither )

-- Internal imports: auxiliary
import Command.Result ( Result (..) )
import Data.Location  ( Location (..) )

-- Internal imports: FRET files and conversion to Copilot
import qualified Language.FRETReqsDB.AST           as FRET ( FRETReqsDB )
import qualified Language.Trans.FRETReqsDB2Copilot as T ( fret2CopilotModule )

-- | Print the contents of a Copilot module that implements the Past-time TL
-- formula in a FRET file.
--
-- PRE: The file given is readable, contains a valid FRET file with a PT
-- formula in the @ptExpanded@ key, the formula does not use any identifiers
-- that exist in Copilot, or any of @prop@, @clock@, @ftp@. All identifiers
-- used are valid C99 identifiers.
fretReqsDB2Copilot :: FilePath -- ^ Path to a file containing a FRET
                               -- Requirements Database
                   -> Bool     -- ^ Use the CoCoSpec variant of the formula
                   -> IO (Result ErrorCode)
fretReqsDB2Copilot fp useCoCoSpec = do

  -- All of the following operations use Either to return error messages. The
  -- use of the monadic bind to pass arguments from one function to the next
  -- will cause the program to stop at the earliest error.
  --
  -- The guard (checking whether the result is Left or Right) must happen
  -- before the first side effects (output file creation), or the file may be
  -- (over) written even when processing fails.
  fret <- parseFret fp
  case T.fret2CopilotModule useCoCoSpec =<< headEither =<< fret of
    Left msg -> return $ Error ecFretReqsDBError msg (LocationFile fp)
    Right t  -> putStrLn t >> return Success

-- | Parse a JSON file containing a FRET requirement database.
--
-- Returns a 'Left' with an error message if the file does not have the correct
-- format.
--
-- Throws an exception if the file cannot be read.
parseFret :: FilePath -> IO (Either String [FRET.FRETReqsDB])
parseFret fp = do
  content <- B.safeReadFile fp
  return $ eitherDecode =<< content

-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error code used is 1 for user error.
type ErrorCode = Int

-- | Error: the FRET Component Spec file cannot be read due to the file being
-- unreadable or the format being incorrect.
ecFretReqsDBError :: ErrorCode
ecFretReqsDBError = 1
