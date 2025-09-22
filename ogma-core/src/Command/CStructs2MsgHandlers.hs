-- Copyright 2020 United States Government as represented by the Administrator
-- of the National Aeronautics and Space Administration. All Rights Reserved.
--
-- Disclaimers
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at
--
--      https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.
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
