{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
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
-- | Produce an overview of the input files.
module Command.Overview
    ( command
    , CommandOptions(..)
    , OverviewFile(..)
    , CommandSummary(..)
    , CommandSummaryRequirements(..)
    , CommandSummaryDiagram(..)
    , ErrorCode
    )
  where

-- External imports
import Control.Monad        (foldM)
import Control.Monad.Except (runExceptT)
import Data.Aeson           (ToJSON (..))
import GHC.Generics         (Generic)

-- External imports: Ogma
import Data.OgmaSpec (Spec (..))

-- Internal imports
import           Command.Common              (InputFile(..), parseInputFile)
import           Command.Errors              (ErrorCode, ErrorTriplet (..))
import           Command.Result              (Result (..))
import           Data.Diagram.Analysis       (AnalysisResult (..),
                                              analyzeDiagram)
import           Data.ExprPair               (ExprPair(..), ExprPairT(..),
                                              exprPair)
import           Data.Location               (Location (..))
import qualified Data.Spec.Analysis          as SpecAnalysis
import           Data.Spec.Extra             (addMissingIdentifiers)
import qualified Language.Trans.Spec2Copilot as Spec2Copilot

-- | Generate overview of a spec given in an input file.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @prop@, @clock@, @ftp@, @notPreviousNot@. All identifiers
-- used are valid C99 identifiers. The template, if provided, exists and uses
-- the variables needed by the overview application generator. The target
-- directory is writable and there's enough disk space to copy the files over.
command :: CommandOptions -- ^ Customization options
        -> IO (Maybe CommandSummary, Result ErrorCode)
command options = do
    fs <- foldM
            processFile
            (Right emptyCommandSummary)
            (commandInputFiles options)

    return $ commandResult options fs

  where

    processFile :: Either (FilePath, String) CommandSummary
                -> OverviewFile
                -> IO (Either (FilePath, String) CommandSummary)
    processFile acc file = case acc of
      Left _     -> return acc
      Right acc' -> do
        let functions = exprPair (overviewFilePropFormat file)
        c <- command' (overviewFilePath file) file functions
        case c of
          Left msg -> return $ Left (overviewFilePath file, msg)
          Right s  -> return $ Right $ mergeCommandSummary acc' s

-- | Generate overview of a spec given in an input file.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @prop@, @clock@, @ftp@, @notPreviousNot@. All identifiers
-- used are valid C99 identifiers. The template, if provided, exists and uses
-- the variables needed by the overview application generator. The target
-- directory is writable and there's enough disk space to copy the files over.
command' :: FilePath
          -> OverviewFile
          -> ExprPair
          -> IO (Either String CommandSummary)
command' fp options (ExprPair exprT) = do
    res <- runExceptT $
             parseInputFile fp formatName propFormatName propVia exprT
    case res of
      Left (ErrorTriplet _ s _) -> return $ Left s

      Right (InputFileDiagram diagramR) -> do
        analysisResult <- analyzeDiagram diagramR
        pure $ Right $ emptyCommandSummary
                         { commandSummaryDiagrams =
                             [ CommandSummaryDiagram
                                 fp
                                 (numStates analysisResult)
                                 (deterministic analysisResult)
                             ]
                         }

      Right (InputFileSpec spec') -> do
        let specCompleted = addMissingIdentifiers ids spec'
            specAnalyzed  = Spec2Copilot.specAnalyze specCompleted

        specFormalAnalysis <-
          SpecAnalysis.specAnalyze [] replace printExpr specCompleted

        pure $ do
          numExterns  <- length . externalVariables <$> specAnalyzed
          numInternal <- length . internalVariables <$> specAnalyzed
          numReqs     <- length . requirements      <$> specAnalyzed
          numTrues    <- SpecAnalysis.numAlwaysTrue  <$> specFormalAnalysis
          numFalses   <- SpecAnalysis.numAlwaysFalse <$> specFormalAnalysis
          consistent  <- SpecAnalysis.consistent     <$> specFormalAnalysis

          pure $ emptyCommandSummary
                   { commandSummaryRequirements =
                       [ CommandSummaryRequirements
                           fp
                           numExterns
                           numInternal
                           numReqs
                           numTrues
                           numFalses
                           consistent
                      ]
                   }

  where

    formatName     = overviewFileFormat options
    propFormatName = overviewFilePropFormat options
    propVia        = overviewFilePropVia options

    ExprPairT _parse replace printExpr ids _def = exprT

data CommandSummary = CommandSummary
    { commandSummaryRequirements :: [CommandSummaryRequirements]
    , commandSummaryDiagrams     :: [CommandSummaryDiagram]
    }
  deriving (Generic, Show)

instance ToJSON CommandSummary

-- | Summary with empty data.
emptyCommandSummary :: CommandSummary
emptyCommandSummary = CommandSummary [] []

-- | Merge two summaries.
mergeCommandSummary :: CommandSummary -> CommandSummary -> CommandSummary
mergeCommandSummary c1 c2 = CommandSummary
  { commandSummaryRequirements =
      commandSummaryRequirements c1 ++ commandSummaryRequirements c2
  , commandSummaryDiagrams =
      commandSummaryDiagrams c1 ++ commandSummaryDiagrams c2
  }

instance Semigroup CommandSummary where
  (<>) = mergeCommandSummary

instance Monoid CommandSummary where
  mempty  = emptyCommandSummary

-- | Requirement data for inclusion in the summary.
data CommandSummaryRequirements = CommandSummaryRequirements
    { commandRequirementsFile       :: FilePath
    , commandExternalVariables      :: Int
    , commandInternalVariables      :: Int
    , commandRequirements           :: Int
    , commandRequirementsTrue       :: Int
    , commandRequirementsFalse      :: Int
    , commandRequirementsConsistent :: Bool
    }
  deriving (Generic, Show)

instance ToJSON CommandSummaryRequirements

-- | Diagram Data for inclusion in the summary.
data CommandSummaryDiagram = CommandSummaryDiagram
    { commandDiagramFile          :: FilePath
    , commandDiagramNumStates     :: Int
    , commandDiagramDeterministic :: Bool
    }
  deriving (Generic, Show)

instance ToJSON CommandSummaryDiagram

-- | Options used to customize the interpretation of input specifications.
newtype CommandOptions = CommandOptions
  { commandInputFiles :: [ OverviewFile ]
  }

-- | Information about one file in the command options.
data OverviewFile = OverviewFile
  { overviewFilePath       :: FilePath
  , overviewFileFormat     :: String
  , overviewFilePropFormat :: String
  , overviewFilePropVia    :: Maybe String
  }

-- * Error codes

-- | Error: the input file cannot be read due to it being unreadable or the
-- format being incorrect.
ecOverviewError :: ErrorCode
ecOverviewError = 1

-- * Result

-- | Process the result of the transformation function.
commandResult :: CommandOptions
              -> Either (FilePath, String) a
              -> (Maybe a, Result ErrorCode)
commandResult _options result = case result of
  Left (fp, msg) -> (Nothing, Error ecOverviewError msg (LocationFile fp))
  Right t        -> (Just t,  Success)
