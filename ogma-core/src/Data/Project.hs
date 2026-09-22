{-# LANGUAGE DeriveGeneric #-}
-- Copyright 2024 United States Government as represented by the Administrator
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
-- | Ogma projects.
module Data.Project
    ( Project(..)
    , readProject
    )
  where

-- External imports
import           Control.Exception (IOException, try)
import           Data.Aeson        (FromJSON, ToJSON, eitherDecodeStrict')
import qualified Data.ByteString   as BS
import           Data.List         (stripPrefix)
import           GHC.Generics      (Generic)
import           System.Directory  (makeAbsolute)
import           System.FilePath   (isAbsolute, takeDirectory, (</>))

-- Internal imports
import Data.Either.Extra (mapLeft)

data Project = Project
    { projectName           :: Maybe String
    , projectInputFiles     :: [(FilePath, String, String)]
                               -- ^ File, format name, prop name
    , projectVariableFiles  :: Maybe FilePath
    , projectVariableDBFile :: Maybe FilePath
    , projectHandlerFile    :: Maybe FilePath
    , projectCommandPropVia :: Maybe FilePath
    , projectTemplateDir    :: Maybe FilePath
    , projectTargetDir      :: Maybe FilePath
    , projectExtraJSONFile  :: Maybe FilePath
    }
  deriving (Generic, Show)

instance FromJSON Project
instance ToJSON Project

-- | Read a project from a file.
readProject :: FilePath -> IO (Either String Project)
readProject path = do
  bytesResult <- try (BS.readFile path)
  let project = case bytesResult of
                  Left e      -> Left (show (e :: IOException))
                  Right bytes -> mapLeft ("Failed to read project: " ++)
                               $ eitherDecodeStrict' bytes

  projectDir <- takeDirectory <$> makeAbsolute path

  pure $ resolveProjectPaths projectDir <$> project

-- | Resolve paths inside a project.
resolveProjectPaths :: FilePath -> Project -> Project
resolveProjectPaths projectDir p = p
  { projectInputFiles     = resolveInputFile projectDir <$> projectInputFiles p
  , projectVariableFiles  = resolvePath projectDir <$> projectVariableFiles p
  , projectVariableDBFile = resolvePath projectDir <$> projectVariableDBFile p
  , projectHandlerFile    = resolvePath projectDir <$> projectHandlerFile p
  , projectCommandPropVia = resolvePath projectDir <$> projectCommandPropVia p
  , projectTemplateDir    = resolvePath projectDir <$> projectTemplateDir p
  , projectTargetDir      = resolvePath projectDir <$> projectTargetDir p
  , projectExtraJSONFile  = resolvePath projectDir <$> projectExtraJSONFile p
  }

-- | Resolve the locations of input file pairs, possibly in relation to the
-- project path.
resolveInputFile :: FilePath
                 -> (FilePath, String, String)
                 -> (FilePath, String, String)
resolveInputFile projectDir (filePath, fileFormat, propName) =
    (filePath', fileFormat', propName)
  where
    filePath'   = resolvePath projectDir filePath
    fileFormat' = resolvePath projectDir fileFormat

-- | Resolve the path to a file, possibly in relation to the project path.
resolvePath :: FilePath -> FilePath -> FilePath
resolvePath projectDir path
  | isAbsolute path
  = path
  | Just path' <- stripPrefix "cwd:" path
  = path'
  | Just path' <- stripPrefix "cmd:" path
  = path'
  | Just path' <- stripPrefix "project:" path
  = projectDir </> path'
  | otherwise
  = projectDir </> path
