{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- Copyright 2022 United States Government as represented by the Administrator
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
-- | Create <https://github.com/nasa/fprime FPrime> components that subscribe
-- to obtain data and call Copilot when new values arrive.

{- HLINT ignore "Functor law" -}
module Command.FPrimeApp
    ( command
    , CommandOptions(..)
    , ErrorCode
    )
  where

-- External imports
import           Control.Applicative    ( liftA2, (<|>) )
import qualified Control.Exception      as E
import           Control.Monad.Except   ( ExceptT(..), liftEither )
import           Data.Aeson             ( ToJSON, toJSON )
import           Data.Char              ( toUpper )
import           Data.Maybe             ( fromMaybe, mapMaybe, maybeToList )
import           GHC.Generics           ( Generic )

-- External imports: auxiliary
import System.Directory.Extra ( copyTemplate )

import qualified Command.Standalone

-- Internal imports: auxiliary
import Command.Result (Result (..))

-- Internal imports
import Command.Common
import Command.Errors     (ErrorCode, ErrorTriplet (..))
import Command.VariableDB (InputDef (..), TypeDef (..), VariableDB, findInput,
                           findType, findTypeByType)

-- | Generate a new FPrime component connected to Copilot.
command :: CommandOptions -- ^ Options to the ROS backend.
        -> IO (Result ErrorCode)
command options = processResult $ do
    -- Obtain template dir
    templateDir <- locateTemplateDir mTemplateDir "fprime"

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

command' :: CommandOptions
         -> ExprPair
         -> ExceptT ErrorTriplet IO AppData
command' options (ExprPair exprT) = do
    -- Open files needed to fill in details in the template.
    vs    <- parseVariablesFile varNameFile
    rs    <- parseRequirementsListFile handlersFile
    varDB <- openVarDBFilesWithDefault varDBFile

    specT <- maybe (return Nothing) (\e -> Just <$> parseInputExpr' e) cExpr
    specF <- maybe (return Nothing) (\f -> Just <$> parseInputFile' f) fp

    let spec = specT <|> specF

    liftEither $ checkArguments spec vs rs

    copilotM <- sequenceA $ (\spec' -> processSpec spec' fp cExpr) <$> spec

    let varNames = fromMaybe (specExtractExternalVariables spec) vs
        monitors = maybe
                     (specExtractHandlers spec)
                     (map (\x -> (x, Nothing)))
                     rs

    let appData   = AppData variables monitors' copilotM
        variables = mapMaybe (variableMap varDB) varNames
        monitors' = mapMaybe (monitorMap varDB) monitors

    return appData

  where

    cExpr          = commandConditionExpr options
    fp             = commandInputFile options
    varNameFile    = commandVariables options
    varDBFile      = maybeToList $ commandVariableDB options
    handlersFile   = commandHandlers options
    formatName     = commandFormat options
    propFormatName = commandPropFormat options
    propVia        = commandPropVia options

    parseInputExpr' e =
      parseInputExpr e propFormatName propVia exprT

    parseInputFile' f =
      parseInputFile f formatName propFormatName propVia exprT

    processSpec spec' expr' fp' =
      Command.Standalone.commandLogic expr' fp' "copilot" [] exprT spec'

-- ** Argument processing

-- | Options used to customize the conversion of specifications to F'
-- applications.
data CommandOptions = CommandOptions
  { commandConditionExpr :: Maybe String   -- ^ Trigger condition.
  , commandInputFile   :: Maybe FilePath -- ^ Input specification file.
  , commandTargetDir   :: FilePath       -- ^ Target directory where the
                                         -- component should be created.
  , commandTemplateDir :: Maybe FilePath -- ^ Directory where the template is
                                         -- to be found.
  , commandVariables   :: Maybe FilePath -- ^ File containing a list of
                                         -- variables to make available to
                                         -- Copilot.
  , commandVariableDB  :: Maybe FilePath -- ^ File containing a list of known
                                         -- variables with their types and the
                                         -- message IDs they can be obtained
                                         -- from.
  , commandHandlers    :: Maybe FilePath -- ^ File containing a list of
                                         -- handlers used in the Copilot
                                         -- specification. The handlers are
                                         -- assumed to receive no arguments.
  , commandFormat      :: String         -- ^ Format of the input file.
  , commandPropFormat  :: String         -- ^ Format used for input properties.
  , commandPropVia     :: Maybe String   -- ^ Use external command to
                                         -- pre-process system properties.
  , commandExtraVars   :: Maybe FilePath -- ^ File containing additional
                                         -- variables to make available to the
                                         -- template.
  }

-- | Return the variable information needed to generate declarations
-- and subscriptions for a given variable name and variable database.
variableMap :: VariableDB
            -> String
            -> Maybe VarDecl
variableMap varDB varName = do
  inputDef     <- findInput varDB varName
  inputDefType <- inputType inputDef
  let typeDef = findType varDB varName "fprime/port" "C"

  portType <- maybe (inputType inputDef) (Just . typeFromType) typeDef

  return $ VarDecl varName inputDefType portType

-- | Return the monitor information needed to generate declarations and
-- publishers for the given monitor info, and variable database.
monitorMap :: VariableDB
           -> (String, Maybe String)
           -> Maybe Monitor
monitorMap varDB (monitorName, Nothing) =
  Just $ Monitor monitorName (map toUpper monitorName) Nothing Nothing
monitorMap varDB (monitorName, Just ty) = do
  let tyPort = maybe ty typeFromType $ findTypeByType varDB "fprime/port" "C" ty
  return $ Monitor monitorName (map toUpper monitorName) (Just ty) (Just tyPort)

-- | The declaration of a variable in C, with a given type and name.
data VarDecl = VarDecl
    { varDeclName       :: String
    , varDeclType       :: String
    , varDeclFPrimeType :: String
    }
  deriving Generic

instance ToJSON VarDecl

data Monitor = Monitor
    { monitorName     :: String
    , monitorUC       :: String
    , monitorType     :: Maybe String
    , monitorPortType :: Maybe String
    }
  deriving Generic

instance ToJSON Monitor

-- | Data that may be relevant to generate a ROS application.
data AppData = AppData
  { variables :: [VarDecl]
  , monitors  :: [Monitor]
  , copilot   :: Maybe Command.Standalone.AppData
  }
  deriving (Generic)

instance ToJSON AppData
