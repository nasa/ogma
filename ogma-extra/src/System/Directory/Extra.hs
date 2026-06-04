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
    ( CopyTemplateError (..)
    , copyTemplate
    )
  where

-- External imports
import           Control.Exception         ( Exception, IOException, handle,
                                             throwIO )
import           Control.Monad             ( filterM, forM_ )
import           Data.Aeson                ( Value (..) )
import qualified Data.ByteString.Lazy      as B
import           Data.Text.Lazy            ( pack, unpack )
import           Data.Text.Lazy.Encoding   ( encodeUtf8 )
import           Distribution.Simple.Utils ( getDirectoryContentsRecursive )
import           GHC.IO.Exception          ( IOErrorType (InvalidArgument) )
import           System.Directory          ( createDirectoryIfMissing,
                                             doesFileExist )
import           System.FilePath           ( makeRelative, splitFileName,
                                             takeDirectory, (</>) )
import           System.IO.Error           ( ioeGetErrorType )
import           Text.Microstache          ( MustacheException (..), Template,
                                             compileMustacheFile,
                                             compileMustacheText,
                                             renderMustache )

-- | Exception thrown when a template cannot be expanded into a target
-- location, indicating the reason for the failure and the file that caused
-- it.
data CopyTemplateError
    = CopyTemplateListError FilePath String
      -- ^ The template directory cannot be listed or read.
    | CopyTemplateReadError FilePath String
      -- ^ A file in the template cannot be opened or read.
    | CopyTemplateDecodeError FilePath String
      -- ^ A file in the template is not a valid UTF-8 text file.
    | CopyTemplateParseError FilePath String
      -- ^ A file in the template contains invalid template syntax.
    | CopyTemplateWriteError FilePath String
      -- ^ A file cannot be written to the target location.
  deriving Show

instance Exception CopyTemplateError

-- | Copy a template directory into a target location, expanding variables
-- provided in a map in a JSON value, both in the file contents and in the
-- filepaths themselves.
--
-- This function throws a 'CopyTemplateError' if the template directory
-- cannot be read, a file in the template cannot be read, decoded or parsed,
-- or a file cannot be written to the target location.
copyTemplate :: FilePath -> Value -> FilePath -> IO ()
copyTemplate templateDir subst targetDir = do

    -- Get all files (not directories) in the template dir. To keep a
    -- directory, create an empty file in it (e.g., .keep).
    tmplContents <- mapIOError (CopyTemplateListError templateDir) $
                      map (templateDir </>) . filter (`notElem` ["..", "."])
                        <$> getDirectoryContentsRecursive templateDir
    tmplFiles <- mapIOError (CopyTemplateListError templateDir) $
                   filterM doesFileExist tmplContents

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
      template <- compileTemplateFile fp
      let contents = encodeUtf8 $ renderMustache template subst

      mapIOError (CopyTemplateWriteError fullPath) $ do
        -- Create target directory if necessary
        let dirName = fst $ splitFileName fullPath
        createDirectoryIfMissing True dirName

        -- Write expanded contents to expanded file path
        B.writeFile fullPath contents

  where

    -- Run an action, turning any IO exception into a 'CopyTemplateError'
    -- using the function provided.
    mapIOError :: (String -> CopyTemplateError) -> IO a -> IO a
    mapIOError mkError = handle $ \e ->
      throwIO $ mkError $ show (e :: IOException)

    -- Compile a file as a mustache template, indicating the reason for the
    -- failure and the offending file if the file cannot be read, decoded, or
    -- parsed.
    compileTemplateFile :: FilePath -> IO Template
    compileTemplateFile fp =
        handle mapMustacheError $ handle mapIOException $
          compileMustacheFile fp
      where
        mapIOException :: IOException -> IO a
        mapIOException e
          | ioeGetErrorType e == InvalidArgument
          = throwIO $ CopyTemplateDecodeError fp (show e)
          | otherwise
          = throwIO $ CopyTemplateReadError fp (show e)

        mapMustacheError :: MustacheException -> IO a
        mapMustacheError (MustacheParserException e) =
          throwIO $ CopyTemplateParseError fp (show e)
        mapMustacheError e =
          throwIO $ CopyTemplateParseError fp (show e)
