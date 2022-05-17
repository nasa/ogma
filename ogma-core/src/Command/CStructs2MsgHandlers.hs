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
-- | Generate C methods that process NASA Core Flight System messages dealing
-- with the structs defined in a header file.
--
-- This module makes use of "Language.Trans.CStructs2MsgHandlers", which does
-- most of the work.
module Command.CStructs2MsgHandlers
    ( cstructs2MsgHandlers
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

-- Internal imports: transformation of C structs to handling methods.
import qualified Language.Trans.CStructs2MsgHandlers as T ( cstructs2MsgHandlers )

-- | Print message handlers that copy data and make it available to Copilot.
cstructs2MsgHandlers :: FilePath -- ^ Path to a readable, valid C header file
                                 -- containing struct definitions.
                     -> IO (Result ErrorCode)
cstructs2MsgHandlers fp = do
    result <- parseCFile fp

    case T.cstructs2MsgHandlers =<< result of
      Right content -> putStrLn content >> return Success

      Left msg -> return $ Error ecCStructError msg (LocationFile fp)

  where

    -- Parse a C file, returning 'Left' with some message when there is a parse
    -- error.
    --
    parseCFile :: FilePath -> IO (Either String C.TranslationUnit)
    parseCFile fp' = do
      content <- S.safeReadFile fp'
      return $ C.pTranslationUnit . C.myLexer =<< content

-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error code used is 1 for user error.
type ErrorCode = Int

-- | Error: the C header file cannot be read due to the file being unreadable
-- or the format being incorrect.
ecCStructError :: ErrorCode
ecCStructError = 1
