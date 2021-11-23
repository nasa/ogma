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
-- | Generate Copilot struct definitions and instances from structs defined in
-- a C header file.
--
-- Working with Copilot structs requires three definitions: the datatype, a
-- @Struct@ instance, and a @Typed@ instance.
--
-- This module converts the C structs into 'Language.Copilot.CStruct.CStruct's,
-- and then converts those 'Language.Copilot.CStruct.CStruct's into Copilot
-- (i.e., Haskell) data type declarations and instance declarations. The result
-- is then printed to a file. This module makes use of
-- "Language.Trans.CStructs2Copilot", which does most of the work.
module Command.CStructs2Copilot
    ( cstructs2Copilot
    , ErrorCode
    )
  where

-- External imports: auxiliary
import Data.String.Extra as S ( safeReadFile )

-- Internal imports: auxiliary
import Command.Result ( Result (..) )
import Data.Location  ( Location (..) )

-- Internal imports: C parsing and AST
import qualified Language.C.AbsC as C ( TranslationUnit )
import qualified Language.C.ParC as C ( myLexer, pTranslationUnit )

-- Internal imports: transformation of C structs to Copilot structs
import Language.Trans.CStructs2Copilot ( cstructs2CopilotDecls )

-- | Generate Copilot struct definitions and instances from structs defined in
-- a C header file.
cstructs2Copilot :: FilePath -- ^ Path to a readable, valid C header file
                             -- containing struct definitions.
                 -> IO (Result ErrorCode)
cstructs2Copilot fp = do
    source <- parseCFile fp

    case cstructs2CopilotDecls =<< source of
      Right decls -> printDecls decls >> return Success
      Left msg    -> return $ Error ecCStructError msg (LocationFile fp)

  where

    -- Parse a C file, returning 'Left' with some message when there is a parse
    -- error.
    --
    parseCFile :: FilePath -> IO (Either String C.TranslationUnit)
    parseCFile fp' = do
      content <- S.safeReadFile fp'
      return $ C.pTranslationUnit . C.myLexer =<< content

    -- Print several Haskell declarations to standard output.
    printDecls :: [ String ] -> IO ()
    printDecls = putStrLn . unlines


-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error code used is 1 for user error.
type ErrorCode = Int

-- | Error: the C header file cannot be read due to the file being unreadable
-- or the format being incorrect.
ecCStructError :: ErrorCode
ecCStructError = 1
