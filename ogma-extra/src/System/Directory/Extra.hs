{-# LANGUAGE OverloadedStrings #-}
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
-- | Auxiliary functions for working with directories.
module System.Directory.Extra
    ( copyDirectoryRecursive
    , copyFile'
    , copyTemplate
    )
  where

-- External imports
import           Control.Monad             ( filterM, forM_ )
import qualified Control.Exception         as E
import           Data.Aeson                ( Value (..) )
import qualified Data.ByteString.Lazy      as B
import           Data.Text.Lazy            ( pack, unpack )
import           Data.Text.Lazy.Encoding   ( encodeUtf8 )
import           Distribution.Simple.Utils ( getDirectoryContentsRecursive )
import           System.Directory          ( copyFile,
                                             createDirectoryIfMissing,
                                             doesFileExist )
import           System.Exit               ( ExitCode (ExitFailure), exitWith )
import           System.FilePath           ( makeRelative, splitFileName,
                                             takeDirectory, (</>) )
import           System.IO                 ( hPutStrLn, stderr )
import           Text.Microstache          ( compileMustacheFile,
                                             compileMustacheText,
                                             renderMustache )

-- | Copy all files from one directory to another.
copyDirectoryRecursive :: FilePath  -- ^ Source directory
                       -> FilePath  -- ^ Target directory
                       -> IO ()
copyDirectoryRecursive sourceDir targetDir =
  E.handle (copyDirectoryRecursiveErrorHandler sourceDir targetDir) $ do
    -- Obtain files in source directory
    files <- getDirectoryContentsRecursive sourceDir

    -- Determine the actual source and destination path for a file.
    let sourceAndDest file = (src, dest)
          where
            src  = sourceDir </> file
            dest = targetDir </> file

    -- Copy all the files, replacing the top directory.
    mapM_ (copyFile' . sourceAndDest) files

-- | Copy file origin to dest, creating the target directory if it does not
-- exist.
copyFile' :: (FilePath, FilePath) -> IO ()
copyFile' (origin, dest) = do
  createDirectoryIfMissing True (takeDirectory dest)
  copyFile origin dest

-- | Handle the case in which the source directory cannot be copied or the
-- target directory cannot be created/written.
copyDirectoryRecursiveErrorHandler :: FilePath
                                   -> FilePath
                                   -> E.SomeException
                                   -> IO ()
copyDirectoryRecursiveErrorHandler sourceDir targetDir _exception = do
  hPutStrLn stderr $
    "ogma: error: cannot copy " ++ sourceDir ++ " to " ++ targetDir
  exitWith (ExitFailure 1)

-- * Generic template handling

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
