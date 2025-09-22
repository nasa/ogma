{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
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
-- | Transform a specification into a standalone Copilot specification.
module Command.Standalone
    ( command
    , commandLogic
    , AppData
    , CommandOptions(..)
    , ErrorCode
    )
  where

-- External imports
import Control.Applicative  ((<|>))
import Control.Exception    as E
import Control.Monad.Except (ExceptT (..), liftEither)
import Data.Aeson           (ToJSON (..))
import Data.List            (nub, (\\))
import Data.Maybe           (fromMaybe)
import GHC.Generics         (Generic)

-- External imports: Ogma
import Data.OgmaSpec          (ExternalVariableDef (..),
                               InternalVariableDef (..), Requirement (..),
                               Spec (..))
import System.Directory.Extra (copyTemplate)

-- Internal imports
import Command.Common
import Command.Errors              (ErrorCode, ErrorTriplet(..))
import Command.Result              (Result (..))
import Data.Location               (Location (..))
import Language.Trans.Spec2Copilot (spec2Copilot, specAnalyze)

-- | Generate a new standalone Copilot monitor that implements the spec in an
-- input file.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @prop@, @clock@, @ftp@, @notPreviousNot@. All identifiers
-- used are valid C99 identifiers. The template, if provided, exists and uses
-- the variables needed by the standalone application generator. The target
-- directory is writable and there's enough disk space to copy the files over.
command :: CommandOptions -- ^ Customization options
        -> IO (Result ErrorCode)
command options = processResult $ do
    -- Obtain template dir
    templateDir <- locateTemplateDir mTemplateDir "standalone"

    templateVars <- parseTemplateVarsFile templateVarsF

    appData <- command' options functions

    let subst = mergeObjects (toJSON appData) templateVars

    -- Expand template
    ExceptT $ fmap (makeLeftE cannotCopyTemplate) $ E.try $
      copyTemplate templateDir subst targetDir

  where

    targetDir     = commandTargetDir options
    mTemplateDir  = commandTemplateDir options
    functions     = exprPair (commandPropFormat options)
    templateVarsF = commandExtraVars options

-- | Generate a new standalone Copilot monitor that implements the spec in an
-- input file, using a subexpression handler.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @prop@, @clock@, @ftp@, @notPreviousNot@. All identifiers
-- used are valid C99 identifiers. The template, if provided, exists and uses
-- the variables needed by the standalone application generator. The target
-- directory is writable and there's enough disk space to copy the files over.
command' :: CommandOptions
         -> ExprPair
         -> ExceptT ErrorTriplet IO AppData
command' options (ExprPair exprT) = do

    -- Read spec and complement the specification with any missing/implicit
    -- definitions.
    specT <- maybe (return Nothing) (\e -> Just <$> parseInputExpr' e) triggerExprM
    specF <- maybe (return Nothing) (\f -> Just <$> parseInputFile' f) fpM
    let spec = specT <|> specF

    case spec of
      Nothing    -> liftEither $ Left $ commandMissingSpec
      Just spec' -> commandLogic triggerExprM fpM name typeMaps exprT spec'

  where
    triggerExprM   = commandConditionExpr options
    fpM            = commandInputFile options
    name           = commandFilename options
    typeMaps       = typeToCopilotTypeMapping (commandTypeMapping options)
    formatName     = commandFormat options
    propFormatName = commandPropFormat options
    propVia        = commandPropVia options

    parseInputExpr' e =
      parseInputExpr e propFormatName propVia exprT

    parseInputFile' f =
      parseInputFile f formatName propFormatName propVia exprT


-- | Generate the data of a new standalone Copilot monitor that implements the
-- spec, using a subexpression handler.
commandLogic :: Maybe String
             -> Maybe FilePath
             -> String
             -> [(String, String)]
             -> ExprPairT a
             -> Spec a
             -> ExceptT ErrorTriplet IO AppData
commandLogic expr fp name typeMaps exprT input = do
    let spec = addMissingIdentifiers ids input
    -- Analyze the spec for incorrect identifiers and convert it to Copilot.
    -- If there is an error, we change the error to a message we control.
    let appData = mapLeft commandIncorrectSpec' $ do
          spec' <- specAnalyze spec
          res   <- spec2Copilot name typeMaps replace print spec'

          -- Pack the results
          let (ext, int, reqs, trigs, specN) = res

          return $ AppData ext int reqs trigs specN

    liftEither appData

  where

    commandIncorrectSpec' = case (expr, fp) of
      (Nothing,    Just fp') -> commandIncorrectSpecF fp'
      (Just expr', _)        -> commandIncorrectSpecE expr'
      (_, _)                 -> error "Both expression and file are missing"

    ExprPairT parse replace print ids def = exprT

-- ** Argument processing

-- | Options used to customize the conversion of specifications to Copilot
-- code.
data CommandOptions = CommandOptions
  { commandConditionExpr :: Maybe String
  , commandInputFile   :: Maybe FilePath     -- ^ Input specification file.
  , commandTargetDir   :: FilePath           -- ^ Target directory where the
                                             -- application should be created.
  , commandTemplateDir :: Maybe FilePath     -- ^ Directory where the template
                                             -- is to be found.
  , commandFormat      :: String             -- ^ Format of the input file.
  , commandPropFormat  :: String             -- ^ Format used for input
                                             -- properties.
  , commandTypeMapping :: [(String, String)]
  , commandFilename    :: String
  , commandPropVia     :: Maybe String       -- ^ Use external command to
                                             -- pre-process system properties.
  , commandExtraVars   :: Maybe FilePath -- ^ File containing additional
                                         -- variables to make available to the
                                         -- template.
  }

-- * Mapping of types from input format to Copilot
typeToCopilotTypeMapping :: [(String, String)] -> [(String, String)]
typeToCopilotTypeMapping types =
    [ ("bool",    "Bool")
    , ("int",     intType)
    , ("integer", intType)
    , ("real",    realType)
    , ("string",  "String")
    , ("",        "_")
    ]
  where
    intType  = fromMaybe "Int64" $ lookup "int" types
    realType = fromMaybe "Float" $ lookup "real" types

-- | Data that may be relevant to generate a ROS application.
data AppData = AppData
    { externs   :: String
    , internals :: String
    , reqs      :: String
    , triggers  :: String
    , specName  :: String
    }
  deriving (Generic)

instance ToJSON AppData

-- | Error message associated to not having a spec of any kind.
commandMissingSpec :: ErrorTriplet
commandMissingSpec =
    ErrorTriplet ecMissingSpec msg LocationNothing
  where
    msg =
      "No input specification has been provided."

-- | Error message associated to not being able to formalize the input spec.
commandIncorrectSpecF :: String -> String -> ErrorTriplet
commandIncorrectSpecF file e =
    ErrorTriplet ecIncorrectSpec msg (LocationFile file)
  where
    msg =
      "The input specification " ++ file ++ " canbot be formalized: " ++ e

-- | Error message associated to not being able to formalize the input spec.
commandIncorrectSpecE :: String -> String -> ErrorTriplet
commandIncorrectSpecE expr e =
    ErrorTriplet ecIncorrectSpec msg LocationNothing
  where
    msg =
      "The input specification " ++ show expr ++ " cannot be formalized: " ++ e

-- ** Error codes

-- | Error: there is no input argument.
ecMissingSpec :: ErrorCode
ecMissingSpec = 1

-- | Error: the input specification cannot be formalized.
ecIncorrectSpec :: ErrorCode
ecIncorrectSpec = 1

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

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x)  = Left (f x)
mapLeft _ (Right x) = Right x
