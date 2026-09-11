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
-- | Produce a report of the diagrams and requirements in the input files.
module Command.Report
    ( command
    , CommandOptions(..)
    , ReportFile(..)
    , CommandSummary(..)
    , ErrorCode
    )
  where

-- External imports
import qualified Control.Exception      as E
import           Control.Monad          (foldM)
import           Control.Monad.Except   (ExceptT (..), liftEither, runExceptT,
                                         withExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (ToJSON (..))
import           GHC.Generics           (Generic)

-- External imports: Ogma
import Data.Either.Extra      (mapLeft)
import Data.OgmaSpec          (Requirement (..), Spec (..))
import Data.String.Extra      (sanitizeUCIdentifier)
import System.Directory.Extra (copyTemplate)

-- Internal imports
import           Command.Common              (InputFile (..),
                                              cannotCopyTemplateF,
                                              locateTemplateDir,
                                              parseInputFile, processResult)
import           Command.Errors              (ErrorCode, ErrorTriplet (..))
import           Command.Result              (Result (..))
import           Data.Diagram.Analysis       (AnalysisResult (..),
                                              analyzeDiagram)
import           Data.ExprPair               (ExprPair (..), ExprPairT (..),
                                              exprPair)
import           Data.Location               (Location (..))
import qualified Data.Spec.Analysis          as SpecAnalysis
import           Data.Spec.Extra             (addMissingIdentifiers)
import qualified Language.Trans.Spec2Copilot as Spec2Copilot

-- | Generate report of a spec or diagram given in an input file.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @prop@, @clock@, @ftp@, @notPreviousNot@. The template,
-- if provided, exists and uses the variables needed by the report application
-- generator. The target directory is writable and there's enough disk space to
-- copy the files over.
command :: CommandOptions -- ^ Customization options
        -> IO (Result ErrorCode)
command options = processResult $ do
    -- Obtain template dir
    templateDir <- locateTemplateDir mTemplateDir "report"

    reportData <- foldM
                    processFile
                    emptyCommandSummary
                    (commandInputFiles options)

    -- Expand template
    ExceptT $ fmap (mapLeft cannotCopyTemplateF) $ E.try $
      copyTemplate templateDir (toJSON reportData) targetDir

  where

    targetDir    = commandTargetDir options
    mTemplateDir = commandTemplateDir options

    processFile :: CommandSummary
                -> ReportFile
                -> ExceptT ErrorTriplet IO CommandSummary
    processFile acc file = do
      let functions = exprPair (reportFilePropFormat file)
      s <- command' options functions file
      return $ mergeCommandSummary acc s

-- | Generate report of a spec or diagram given in an input file.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @prop@, @clock@, @ftp@, @notPreviousNot@. The template,
-- if provided, exists and uses the variables needed by the report application
-- generator. The target directory is writable and there's enough disk space to
-- copy the files over.
command' :: CommandOptions
         -> ExprPair
         -> ReportFile
         -> ExceptT ErrorTriplet IO CommandSummary
command' options (ExprPair exprT) file = do
    res <- parseInputFile fp formatName propFormatName propVia exprT
    case res of
      InputFileDiagram diagramR -> do
        analysisResult <- liftIO $ analyzeDiagram diagramR
        let diagramDetails = CommandSummaryDiagram
                               fp
                               (numStates analysisResult)
                               (deterministic analysisResult)

        pure $ CommandSummary
                 { commandRequirementsAny = False
                 , commandRequirementList = []
                 , commandDiagramsList    = [ diagramDetails ]
                 , commandDiagramsAny     = True
                 }

      InputFileSpec spec -> withExceptT commandCannotAnalyzeF $ do
        let specCompleted = addMissingIdentifiers ids spec
        specAnalyzed <- liftEither $ Spec2Copilot.specAnalyze specCompleted

        specFormalAnalysis <- ExceptT $
          SpecAnalysis.specAnalyze [] replace printExpr specCompleted

        let numExterns  = length $ externalVariables specAnalyzed
            numInternal = length $ internalVariables specAnalyzed

            numReqs        = length reqList
            reqList        = requirements specAnalyzed
            reqListDetails = map reqDetailsF reqList
            reqDetailsF x  =
              RequirementDetails
                (requirementName x)
                (requirementDescription x)
                (requirementNameAsProp (requirementName x) `elem` trueReqs)
                (requirementNameAsProp (requirementName x) `elem` falseReqs)

            numTrues   = SpecAnalysis.numAlwaysTrue  specFormalAnalysis
            numFalses  = SpecAnalysis.numAlwaysFalse specFormalAnalysis
            trueReqs   = SpecAnalysis.alwaysTrueReq  specFormalAnalysis
            falseReqs  = SpecAnalysis.alwaysFalseReq specFormalAnalysis
            consistent = SpecAnalysis.consistent     specFormalAnalysis

            fileReqs   = CommandSummaryRequirements
                           { summaryRequirementsFile       = fp
                           , summaryExternalVariables      = numExterns
                           , summaryInternalVariables      = numInternal
                           , summaryRequirements           = numReqs
                           , summaryRequirementsTrue       = numTrues
                           , summaryRequirementsFalse      = numFalses
                           , summaryRequirementsConsistent = consistent
                           , summaryRequirementsDetails    = reqListDetails
                           }

        pure $ CommandSummary
                 { commandRequirementsAny = not (null reqListDetails)
                 , commandRequirementList = [fileReqs]
                 , commandDiagramsAny     = False
                 , commandDiagramsList    = []
                 }

  where

    fp             = reportFilePath file
    formatName     = reportFileFormat file
    propFormatName = reportFilePropFormat file
    propVia        = reportFilePropVia file

    ExprPairT _parse replace printExpr ids _def = exprT

-- | Options used to customize the interpretation of input specifications and
-- the resulting report.
data CommandOptions = CommandOptions
  { commandTargetDir   :: String
  , commandTemplateDir :: Maybe String
  , commandInputFiles  :: [ ReportFile ]
  }

-- | Information about one file in the command options.
data ReportFile = ReportFile
  { reportFilePath       :: FilePath
  , reportFileFormat     :: String
  , reportFilePropFormat :: String
  , reportFilePropVia    :: Maybe String
  }

-- | Summary of the files read.
data CommandSummary = CommandSummary
    { commandRequirementsAny :: Bool
    , commandRequirementList :: [CommandSummaryRequirements]
    , commandDiagramsAny     :: Bool
    , commandDiagramsList    :: [CommandSummaryDiagram]
    }
  deriving (Generic, Show)

instance ToJSON CommandSummary

-- | Summary with empty data.
emptyCommandSummary :: CommandSummary
emptyCommandSummary = CommandSummary False [] False []

-- | Merge two summaries.
mergeCommandSummary :: CommandSummary -> CommandSummary -> CommandSummary
mergeCommandSummary c1 c2 = CommandSummary
  { commandRequirementsAny =
      commandRequirementsAny c1 || commandRequirementsAny c2
  , commandRequirementList =
      commandRequirementList c1 ++ commandRequirementList c2
  , commandDiagramsAny =
      commandDiagramsAny c1 || commandDiagramsAny c2
  , commandDiagramsList =
      commandDiagramsList c1 ++ commandDiagramsList c2
  }

-- | Requirement data for inclusion in the summary.
data CommandSummaryRequirements = CommandSummaryRequirements
    { summaryRequirementsFile       :: FilePath
    , summaryExternalVariables      :: Int
    , summaryInternalVariables      :: Int
    , summaryRequirements           :: Int
    , summaryRequirementsTrue       :: Int
    , summaryRequirementsFalse      :: Int
    , summaryRequirementsConsistent :: Bool
    , summaryRequirementsDetails    :: [RequirementDetails]
    }
  deriving (Generic, Show)

instance ToJSON CommandSummaryRequirements

-- | Information to include in a report about a requirement.
data RequirementDetails = RequirementDetails
    { summaryRequirementName  :: String
    , summaryRequirementDesc  :: String
    , summaryRequirementTrue  :: Bool
    , summaryRequirementFalse :: Bool
    }
  deriving (Generic, Show)

instance ToJSON RequirementDetails

-- | Information to include in a report about a diagram.
data CommandSummaryDiagram = CommandSummaryDiagram
    { summaryDiagramFile          :: FilePath
    , summaryDiagramNumStates     :: Int
    , summaryDiagramDeterministic :: Bool
    }
  deriving (Generic, Show)

instance ToJSON CommandSummaryDiagram

-- * Errors

-- | Error message associated to not being able to formalize the input spec.
commandCannotAnalyzeF :: String -> ErrorTriplet
commandCannotAnalyzeF e =
    ErrorTriplet ecCannotAnalyzeError msg LocationNothing
  where
    msg = "The input specification(s) cannot be analyzed: " ++ e

-- ** Error codes

-- | Error: the input file cannot be analyzed.
ecCannotAnalyzeError :: ErrorCode
ecCannotAnalyzeError = 1

-- * Auxiliary functions

-- | Name of a requirement when used as a property or handler in a
-- Copilot specification.
requirementNameAsProp :: String -> String
requirementNameAsProp x = "prop" ++ sanitizeUCIdentifier x
