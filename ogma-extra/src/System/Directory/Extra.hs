{-# LANGUAGE OverloadedStrings #-}
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
-- | Auxiliary functions for working with directories.
module System.Directory.Extra
    ( copyTemplate
    , CopyTemplateException(..)
    )
  where

-- External imports
import           Control.Exception         ( Exception, IOException, catch,
                                             throwIO )
import           Control.Monad             ( filterM, forM_ )
import           Data.Aeson                ( Value (..) )
import qualified Data.ByteString.Lazy      as B
import           Data.List                 ( isInfixOf )
import           Data.Text.Lazy            ( pack, unpack )
import           Data.Text.Lazy.Encoding   ( encodeUtf8 )
import           Distribution.Simple.Utils ( getDirectoryContentsRecursive )
import           System.Directory          ( createDirectoryIfMissing,
                                             doesFileExist )
import           System.FilePath           ( makeRelative, splitFileName,
                                             (</>) )
import           Text.Microstache          ( MustacheException (..), Template,
                                             compileMustacheFile,
                                             compileMustacheText,
                                             renderMustache )
import           Text.Parsec.Error         ( Message (..), errorMessages,
                                             errorPos )
import           Text.Parsec.Pos           ( sourceColumn, sourceLine )

{- HLINT ignore "Redundant <$>" -}
-- | Copy a template directory into a target location, expanding variables
-- provided in a map in a JSON value, both in the file contents and in the
-- filepaths themselves.
copyTemplate :: FilePath -> Value -> FilePath -> IO ()
copyTemplate templateDir subst targetDir = do

  -- Get all files (not directories) in the template dir. To keep a directory,
  -- create an empty file in it (e.g., .keep).
  tmplContents <- map (templateDir </>) . filter (`notElem` ["..", "."])
                    <$> getDirectoryContentsRecursiveE templateDir

  tmplFiles <- filterM doesFileExist tmplContents

  -- Copy files to new locations, expanding their name and contents as
  -- mustache templates.
  forM_ tmplFiles $ \fp -> do

    -- New file name in target directory, treating file
    -- name as mustache template.
    let fullPath = targetDir </> newFP
          where
            -- If file name has mustache markers, expand, otherwise use
            -- relative file path
            newFP = either (const relFP)
                           (unpack . (`renderMustache` subst))
                           fpAsTemplateE

            -- Local file name within template dir
            relFP = makeRelative templateDir fp

            -- Apply mustache substitutions to file name
            fpAsTemplateE = compileMustacheText "fp" (pack relFP)

    -- File contents, treated as a mustache template.
    contents <- encodeUtf8 <$> (`renderMustache` subst)
                           <$> compileMustacheFileE fp

    -- Create target directory if necessary
    let dirName = fst $ splitFileName fullPath
    createDirectoryIfMissingE True dirName

    -- Write expanded contents to expanded file path
    -- Capture exceptions here
    writeFileE fullPath contents

-- | Exception detected during the template expansion process.
newtype CopyTemplateException = CopyTemplateException String

instance Show CopyTemplateException where
  show (CopyTemplateException s) = s

instance Exception CopyTemplateException

-- | Wrap 'getDirectoryContentsRecursive' and throw any 'IOException' as a
-- 'CopyTemplateException'.
getDirectoryContentsRecursiveE :: FilePath -> IO [FilePath]
getDirectoryContentsRecursiveE s =
    catch (getDirectoryContentsRecursive s) handler
  where
    handler :: IOException -> IO [FilePath]
    handler e = throwIO (CopyTemplateException (show e))

-- | Wrap 'createDirectoryIfMissing' and throw any 'IOException' as a
-- 'CopyTemplateException', possibly making the error message more
-- user-friendly.
createDirectoryIfMissingE :: Bool -> FilePath -> IO ()
createDirectoryIfMissingE parents fp =
    catch (createDirectoryIfMissing parents fp) handler
  where
    handler :: IOException -> IO ()
    handler e
      | "createDirectory: permission denied" `isInfixOf` show e
      = throwIO $ CopyTemplateException $
          fp ++ ": " ++ "Error creating target directory (permission denied)"

      | otherwise
      = throwIO $ CopyTemplateException $ fp ++ ": " ++ show e

-- | Wrap 'writeFile' and throw any 'IOException' as a 'CopyTemplateException',
-- possibly making the error message more user-friendly.
writeFileE :: FilePath -> B.ByteString -> IO ()
writeFileE fp contents =
    catch (B.writeFile fp contents) handler
  where
    handler :: IOException -> IO ()
    handler e
      | "permission denied" `isInfixOf` show e
      = throwIO $ CopyTemplateException $
          fp ++ ": " ++ "Error creating target file (permission denied)"

      | "resource exhausted" `isInfixOf` show e
      = throwIO $ CopyTemplateException $
          fp ++ ": " ++ "No space left on device"

      | otherwise
      = throwIO $ CopyTemplateException $ fp ++ ": " ++ show e

-- | Wrap 'compileMustacheFile' and throw any 'IOException' or
-- 'MustacheException' as a 'CopyTemplateException', possibly making the error
-- message more user-friendly.
compileMustacheFileE :: FilePath -> IO Template
compileMustacheFileE fp = do
    catch (catch (compileMustacheFile fp) handler) handlerIO
  where
    handler :: MustacheException -> IO Template
    handler (MustacheParserException p) = do
      let pos      = errorPos p
          line     = sourceLine pos
          column   = sourceColumn pos
          messages = keepHead $ map showMessage $ errorMessages p
      throwIO $ CopyTemplateException $
        fp ++ ":" ++ show line ++ ":" ++ show column ++ ": " ++ messages

    handler e = do
      throwIO $ CopyTemplateException $ fp ++ ": " ++ show e

    handlerIO :: IOException -> IO Template
    handlerIO e
      | "hGetContents: invalid argument" `isInfixOf` show e
      = throwIO $ CopyTemplateException $
          fp ++ ": " ++ "Invalid UTF-8 byte sequence"

      | "invalid byte sequence" `isInfixOf` show e
      = throwIO $ CopyTemplateException $
          fp ++ ": " ++ "Invalid UTF-8 byte sequence"

      | "openFile: permission denied" `isInfixOf` show e
      = throwIO $ CopyTemplateException $ fp ++ ": " ++ "Permission denied"

      | otherwise
      = throwIO $ CopyTemplateException $ fp ++ ": " ++ show e

-- | Show a parse message.
showMessage :: Message -> String
showMessage (SysUnExpect s) = "Unexpected " ++ s
showMessage (UnExpect s)    = "Unexpected " ++ s
showMessage (Expect s)      = "Expected " ++ s
showMessage (Message s)     = s

-- | Keep the first element of a list of strings, returning the empty string if
-- the list is empty.
keepHead :: [String] -> String
keepHead (a:_) = a
keepHead _     = ""
