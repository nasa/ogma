{-# LANGUAGE DeriveGeneric #-}
-- Copyright 2025 United States Government as represented by the Administrator
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
-- | Find elements in a project that meet a search query.
module Command.Search
    ( command
    , CommandOptions(..)
    , SearchFile(..)
    , CommandSearchResults(..)
    , RequirementInfo(..)
    , DiagramInfo(..)
    , ErrorCode
    )
  where

-- External imports
import Control.Monad        (foldM)
import Control.Monad.Except (runExceptT)
import Data.Aeson           (ToJSON (..))
import Data.List            (isInfixOf)
import GHC.Generics         (Generic)

-- External imports: ogma
import Data.OgmaSpec (Requirement (..), Spec (..))

-- Internal imports
import Command.Common (InputFile (..), parseInputFile)
import Command.Errors (ErrorCode, ErrorTriplet (..))
import Command.Result (Result (..))
import Data.Diagram   (Diagram (..))
import Data.ExprPair  (ExprPair (..), exprPair)
import Data.Location  (Location (..))

-- | Find elements in a project that meet a search query.
command :: CommandOptions -- ^ Customization options
        -> IO (Maybe CommandSearchResults, Result ErrorCode)
command options = do
    fs <- foldM
            processFile
            (Right emptyCommandSearchResults)
            (commandInputFiles options)

    return $ commandResult options fs

  where

    processFile :: Either (FilePath, String) CommandSearchResults
                -> SearchFile
                -> IO (Either (FilePath, String) CommandSearchResults)
    processFile acc file = case acc of
      Left _     -> return acc
      Right acc' -> do
        let functions = exprPair (searchFilePropFormat file)
        c <- command' file functions (commandSearchQuery options)
        case c of
          Left msg -> return $ Left (searchFilePath file, msg)
          Right s  -> return $ Right $ mergeCommandSearchResults acc' s

-- | Find elements in a file that meet a search query.
command' :: SearchFile
         -> ExprPair
         -> String
         -> IO (Either String CommandSearchResults)
command' file (ExprPair exprT) query = do
    res <- runExceptT $
             parseInputFile fp formatName propFormatName propVia exprT

    case res of
      Left (ErrorTriplet _ s _) -> return $ Left s

      Right (InputFileDiagram diagramR) ->
        return $ Right $ diagramResults fp diagramR query

      Right (InputFileSpec spec) ->
        return $ Right $ specResults fp spec query

  where

    fp             = searchFilePath file
    formatName     = searchFileFormat file
    propFormatName = searchFilePropFormat file
    propVia        = searchFilePropVia file

-- | Find elements in a spec that meet a search query.
specResults :: FilePath -> Spec a -> String -> CommandSearchResults
specResults file spec query = CommandSearchResults
  { searchResultRequirements =
      [ RequirementInfo file n d
      | r <- requirements spec
      , let n = requirementName r
      , let d = requirementDescription r
      , query `isInfixOf` n || query `isInfixOf` d
      ]
  , searchResultDiagrams = []
  }

-- | Find elements in a diagram that meet a search query.
diagramResults :: FilePath -> Diagram -> String -> CommandSearchResults
diagramResults file diagramR query = CommandSearchResults
  { searchResultRequirements = []
  , searchResultDiagrams =
      [ DiagramInfo file
      | any (\(f, t, d) -> query `isInfixOf` show f
                        || query `isInfixOf` t
                        || query `isInfixOf` show d)
        (diagramTransitions diagramR)
      ]
  }

-- | Options used to customize the interpretation of input specifications.
data CommandOptions = CommandOptions
  { commandInputFiles  :: [ SearchFile ]
  , commandSearchQuery :: String
  }

-- | Information about one file in the command options.
data SearchFile = SearchFile
  { searchFilePath       :: FilePath
  , searchFileFormat     :: String
  , searchFilePropFormat :: String
  , searchFilePropVia    :: Maybe String
  }

-- | Lists of search results.
data CommandSearchResults = CommandSearchResults
    { searchResultRequirements :: [RequirementInfo]
    , searchResultDiagrams     :: [DiagramInfo]
    }
  deriving (Generic, Show)

instance ToJSON CommandSearchResults

-- | Empty lists of search results.
emptyCommandSearchResults :: CommandSearchResults
emptyCommandSearchResults = CommandSearchResults
  { searchResultRequirements = []
  , searchResultDiagrams     = []
  }

-- | Merge lists of search results.
mergeCommandSearchResults :: CommandSearchResults
                          -> CommandSearchResults
                          -> CommandSearchResults
mergeCommandSearchResults c1 c2 = CommandSearchResults
  { searchResultRequirements =
      searchResultRequirements c1 ++ searchResultRequirements c2
  , searchResultDiagrams =
      searchResultDiagrams c1 ++ searchResultDiagrams c2
  }

-- | Information about a requirement that matches the search query.
data RequirementInfo = RequirementInfo
    { requirementInfoLocation    :: FilePath
    , requirementInfoName        :: String
    , requirementInfoDescription :: String
    }
  deriving (Generic, Show)

instance ToJSON RequirementInfo

-- | Information about a diagram that matches the search query.
newtype DiagramInfo = DiagramInfo
    { diagramInfoLocation :: FilePath
    }
  deriving (Generic, Show)

instance ToJSON DiagramInfo

-- * Error codes

-- | Error: the input file cannot be read due to it being unreadable or the
-- format being incorrect.
ecSearchError :: ErrorCode
ecSearchError = 1

-- * Result

-- | Process the result of the transformation function.
commandResult :: CommandOptions
              -> Either (FilePath, String) a
              -> (Maybe a, Result ErrorCode)
commandResult _options result = case result of
  Left (fp, msg) -> (Nothing, Error ecSearchError msg (LocationFile fp))
  Right t        -> (Just t,  Success)
