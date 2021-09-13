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
    )
  where

-- External imports
import qualified Control.Exception         as E
import           Distribution.Simple.Utils ( getDirectoryContentsRecursive )
import           System.Directory          ( copyFile,
                                             createDirectoryIfMissing )
import           System.Exit               ( ExitCode (ExitFailure), exitWith )
import           System.FilePath           ( takeDirectory, (</>) )
import           System.IO                 ( hPutStrLn, stderr )

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
