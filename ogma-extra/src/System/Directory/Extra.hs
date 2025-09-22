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
    )
  where

-- External imports
import           Control.Monad             ( filterM, forM_ )
import           Data.Aeson                ( Value (..) )
import qualified Data.ByteString.Lazy      as B
import           Data.Text.Lazy            ( pack, unpack )
import           Data.Text.Lazy.Encoding   ( encodeUtf8 )
import           Distribution.Simple.Utils ( getDirectoryContentsRecursive )
import           System.Directory          ( createDirectoryIfMissing,
                                             doesFileExist )
import           System.FilePath           ( makeRelative, splitFileName,
                                             takeDirectory, (</>) )
import           Text.Microstache          ( compileMustacheFile,
                                             compileMustacheText,
                                             renderMustache )

-- | Copy a template directory into a target location, expanding variables
-- provided in a map in a JSON value, both in the file contents and in the
-- filepaths themselves.
copyTemplate :: FilePath -> Value -> FilePath -> IO ()
copyTemplate templateDir subst targetDir = do

  -- Get all files (not directories) in the template dir. To keep a directory,
  -- create an empty file in it (e.g., .keep).
  tmplContents <- map (templateDir </>) . filter (`notElem` ["..", "."])
                    <$> getDirectoryContentsRecursive templateDir
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
                           <$> compileMustacheFile fp

    -- Create target directory if necessary
    let dirName = fst $ splitFileName fullPath
    createDirectoryIfMissing True dirName

    -- Write expanded contents to expanded file path
    B.writeFile fullPath contents
