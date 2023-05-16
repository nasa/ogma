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
-- | Transform a FRET component specification into a R2U2 specification.
--
-- This module makes use of
-- "Language.Trans.FRETComponentSpec2R2U2", which does most of the work.
module Command.FRETComponentSpec2R2U2
    ( fretComponentSpec2R2U2
    , FRETComponentSpec2R2U2Options(..)
    , ErrorCode
    )
  where

-- External imports
import Control.Monad.IfElse ( awhen )
import Data.Aeson           ( eitherDecode )

-- External imports: auxiliary
import Data.ByteString.Extra as B ( safeReadFile )

-- Internal imports: auxiliary
import Command.Result ( Result (..) )
import Data.Location  ( Location (..) )

-- Internal imports
import           Language.FRETComponentSpec.AST           ( FRETComponentSpec )
import qualified Language.Trans.FRETComponentSpec2R2U2 as T
  ( fretComponentSpec2R2U2
  , FRETComponentSpec2R2U2Options(FRETComponentSpec2R2U2Options)
  )

-- | Print the contents of a R2U2 module that implements the Past-time TL
-- formula in a FRET file.
--
-- PRE: The file given is readable, contains a valid FRET file with a PT
-- formula in the @ptExpanded@ key, the formula does not use any identifiers
-- that exist in R2U2, or any of @prop@, @clock@, @ftp@. All identifiers
-- used are valid C99 identifiers.
fretComponentSpec2R2U2 :: FilePath -- ^ Path to a file containing a FRET
                                      -- Component Specification
                          -> FRETComponentSpec2R2U2Options
                                      -- ^ Customization options
                                      -- formula
                          -> IO (Result ErrorCode)
fretComponentSpec2R2U2 fp options = do

  -- All of the following operations use Either to return error messages. The
  -- use of the monadic bind to pass arguments from one function to the next
  -- will cause the program to stop at the earliest error.
  fret <- parseFretComponentSpec fp

  -- Extract internal command options from external command options
  let internalOptions = fretComponentSpec2R2U2Options options

  let r2u2 = T.fretComponentSpec2R2U2 internalOptions =<< fret

  let (mOutput, result) =
        fretComponentSpec2R2U2Result options fp r2u2

  awhen mOutput putStrLn
  return result

-- | Options used to customize the conversion of FRET Component Specifications
-- to R2U2 code.
data FRETComponentSpec2R2U2Options = FRETComponentSpec2R2U2Options
  { fretCS2R2U2UseCoCoSpec :: Bool
  , fretCS2R2U2IntType     :: String
  , fretCS2R2U2RealType    :: String
  }

-- | Parse a JSON file containing a FRET component specification.
--
-- Returns a 'Left' with an error message if the file does not have the correct
-- format.
--
-- Throws an exception if the file cannot be read.
parseFretComponentSpec :: FilePath -> IO (Either String FRETComponentSpec)
parseFretComponentSpec fp = do
  content <- B.safeReadFile fp
  return $ eitherDecode =<< content

-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error code used is 1 for user error.
type ErrorCode = Int

-- | Error: the FRET Component Spec file cannot be read due to the file being
-- unreadable or the format being incorrect.
ecFretCSError :: ErrorCode
ecFretCSError = 1

-- * Input arguments

-- | Convert command input argument options to internal transformation function
-- input arguments.
fretComponentSpec2R2U2Options :: FRETComponentSpec2R2U2Options
                                 -> T.FRETComponentSpec2R2U2Options
fretComponentSpec2R2U2Options options =
  T.FRETComponentSpec2R2U2Options
      (fretCS2R2U2UseCoCoSpec options)
      (fretCS2R2U2IntType options)
      (fretCS2R2U2RealType options)

-- * Result

-- | Process the result of the transformation function.
fretComponentSpec2R2U2Result :: FRETComponentSpec2R2U2Options
                                -> FilePath
                                -> Either String String
                                -> (Maybe String, Result ErrorCode)
fretComponentSpec2R2U2Result options fp result = case result of
  Left msg -> (Nothing, Error ecFretCSError msg (LocationFile fp))
  Right t  -> (Just t, Success)
