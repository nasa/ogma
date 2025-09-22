{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
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
    , CommandSummary(..)
    , ErrorCode
    )
  where

-- External imports
import Control.Monad.Except (runExceptT)
import Data.Aeson           (ToJSON (..))
import Data.List            (nub, (\\))
import GHC.Generics         (Generic)

-- External imports: Ogma
import Data.OgmaSpec (ExternalVariableDef (..), InternalVariableDef (..),
                      Requirement (..), Spec (..))

-- Internal imports
import Command.Common
import Command.Errors              (ErrorCode, ErrorTriplet(..))
import Command.Result              (Result (..))
import Data.Location               (Location (..))
import Language.Trans.Spec2Copilot (specAnalyze)

-- | Generate overview of a spec given in an input file.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @prop@, @clock@, @ftp@, @notPreviousNot@. All identifiers
-- used are valid C99 identifiers. The template, if provided, exists and uses
-- the variables needed by the overview application generator. The target
-- directory is writable and there's enough disk space to copy the files over.
command :: FilePath        -- ^ Path to a file containing a specification
        -> CommandOptions -- ^ Customization options
        -> IO (Maybe CommandSummary, Result ErrorCode)
command fp options = do
  let functions = exprPair (commandPropFormat options)

  copilot <- command' fp options functions

  return $ commandResult options fp copilot

-- | Generate overview of a spec given in an input file.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @prop@, @clock@, @ftp@, @notPreviousNot@. All identifiers
-- used are valid C99 identifiers. The template, if provided, exists and uses
-- the variables needed by the overview application generator. The target
-- directory is writable and there's enough disk space to copy the files over.
command' :: FilePath
          -> CommandOptions
          -> ExprPair
          -> IO (Either String CommandSummary)
command' fp options (ExprPair exprT) = do
    spec <- runExceptT $ parseInputFile' fp
    let spec' = either (\(ErrorTriplet _ec msg _loc) -> Left msg) Right spec

    let summary = do
          spec1 <- spec'
          spec3 <- specAnalyze $ addMissingIdentifiers ids spec1
          return $ CommandSummary (length (externalVariables spec3))
                                  (length (internalVariables spec3))
                                  (length (requirements spec3))
    return summary

  where

    parseInputFile' f = parseInputFile f formatName propFormatName propVia exprT
    formatName        = commandFormat options
    propFormatName    = commandPropFormat options
    propVia           = commandPropVia options

    ExprPairT _parse _replace _print ids _def = exprT

data CommandSummary = CommandSummary
  { commandExternalVariables :: Int
  , commandInternalVariables :: Int
  , commandRequirements      :: Int
  }
  deriving (Generic, Show)

instance ToJSON CommandSummary

-- | Options used to customize the interpretation of input specifications.
data CommandOptions = CommandOptions
  { commandFormat     :: String
  , commandPropFormat :: String
  , commandPropVia    :: Maybe String
  }

-- * Error codes

-- | Error: the input file cannot be read due to it being unreadable or the
-- format being incorrect.
ecOverviewError :: ErrorCode
ecOverviewError = 1

-- * Result

-- | Process the result of the transformation function.
commandResult :: CommandOptions
              -> FilePath
              -> Either String a
              -> (Maybe a, Result ErrorCode)
commandResult _options fp result = case result of
  Left msg -> (Nothing, Error ecOverviewError msg (LocationFile fp))
  Right t  -> (Just t,  Success)

-- | Add to a spec external variables for all identifiers mentioned in
-- expressions that are not defined anywhere.
addMissingIdentifiers :: (a -> [String]) -> Spec a -> Spec a
addMissingIdentifiers f s = s { externalVariables = vars' }
  where
    vars'   = externalVariables s ++ newVars
    newVars = map (\n -> ExternalVariableDef n "") newVarNames

    -- Names that are not defined anywhere
    newVarNames = identifiers \\ existingNames

    -- Identifiers being mentioned in the requirements.
    identifiers = nub $ concatMap (f . requirementExpr) (requirements s)

    -- Names that are defined in variables.
    existingNames = map externalVariableName (externalVariables s)
                 ++ map internalVariableName (internalVariables s)
