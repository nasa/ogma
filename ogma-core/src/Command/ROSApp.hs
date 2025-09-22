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
-- | Create <https://www.ros.org/ Robot Operating System> (ROS) applications
-- that subscribe to obtain data and call Copilot when new values arrive.
--
-- It is the user's responsibility to modify the generated Copilot/C/C++ code
-- to deal with the monitors they'd like to implement, and the data they must
-- manipulate.

{- HLINT ignore "Functor law" -}
module Command.ROSApp
    ( command
    , CommandOptions(..)
    , Node(Node)
    , ErrorCode
    )
  where

-- External imports
import           Control.Applicative  (liftA2, (<|>))
import qualified Control.Exception    as E
import           Control.Monad.Except (ExceptT (..), liftEither)
import           Data.Aeson           (ToJSON (..))
import           Data.Maybe           (fromMaybe, mapMaybe, maybeToList)
import           GHC.Generics         (Generic)

-- External imports: auxiliary
import System.Directory.Extra (copyTemplate)

import qualified Command.Standalone

-- Internal imports: auxiliary
import Command.Result (Result (..))

-- Internal imports
import Command.Common
import Command.Errors     (ErrorCode, ErrorTriplet (..))
import Command.VariableDB (Connection (..), InputDef (..), TopicDef (..),
                           TypeDef (..), VariableDB, findConnection, findInput,
                           findTopic, findType, findTypeByType)

-- | Generate a new ROS application connected to Copilot.
command :: CommandOptions -- ^ Options to the ROS backend.
        -> IO (Result ErrorCode)
command options = processResult $ do
    -- Obtain template dir
    templateDir <- locateTemplateDir mTemplateDir "ros"

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

    let appData =
          AppData variables monitors' copilotM testingAdditionalApps testingVars

        variables = mapMaybe (variableMap varDB) varNames
        monitors' = mapMaybe (monitorMap varDB) monitors

        testingVars
          | null testingLimitedVars
          = variables
          | otherwise
          = filter (\x -> varDeclName x `elem` testingLimitedVars) variables

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

    testingAdditionalApps = commandTestingApps options
    testingLimitedVars    = commandTestingVars options

-- ** Argument processing

-- | Options used to customize the conversion of specifications to ROS
-- applications.
data CommandOptions = CommandOptions
  { commandConditionExpr :: Maybe String   -- ^ Trigger condition.
  , commandInputFile   :: Maybe FilePath -- ^ Input specification file.
  , commandTargetDir   :: FilePath       -- ^ Target directory where the
                                         -- application should be created.
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
  , commandTestingApps :: [Node]         -- ^ Additional applications to turn
                                         -- on during testing.
  , commandTestingVars :: [String]       -- ^ Limited list of variables to use
                                         -- for testing.
  }

-- | Return the variable information needed to generate declarations
-- and subscriptions for a given variable name and variable database.
variableMap :: VariableDB
            -> String
            -> Maybe VarDecl
variableMap varDB varName = do
  inputDef <- findInput varDB varName
  mid      <- connectionTopic <$> findConnection inputDef "ros/message"
  topicDef <- findTopic varDB "ros/message" mid
  typeVar' <- maybe
                (inputType inputDef)
                (Just . typeToType)
                (findType varDB varName "ros/variable" "C")
  let typeMsg' = fromMaybe
                   (topicType topicDef)
                   (typeFromType <$> findType varDB varName "ros/message" "C")
  return $ VarDecl varName typeVar' mid typeMsg' (randomBaseType typeVar')

-- | Return the monitor information needed to generate declarations and
-- publishers for the given monitor info, and variable database.
monitorMap :: VariableDB
           -> (String, Maybe String)
           -> Maybe Monitor
monitorMap varDB (monitorName, Nothing) =
  Just $ Monitor monitorName Nothing Nothing
monitorMap varDB (monitorName, Just ty) = do
  let ty1 = maybe ty typeFromType $ findTypeByType varDB "ros/variable" "C" ty
  ty2 <- typeFromType <$> findTypeByType varDB "ros/message" "C" ty
  return $ Monitor monitorName (Just ty1) (Just ty2)

-- | The declaration of a variable in C, with a given type and name.
data VarDecl = VarDecl
    { varDeclName    :: String
    , varDeclType    :: String
    , varDeclId      :: String
    , varDeclMsgType :: String
    , varDeclRandom  :: String
    }
  deriving Generic

instance ToJSON VarDecl

-- | The name of a handler associated to each condition, and the type
-- of value it receives, together with the type for the message.
data Monitor = Monitor
    { monitorName    :: String
    , monitorType    :: Maybe String
    , monitorMsgType :: Maybe String
    }
  deriving Generic

instance ToJSON Monitor

-- | A package-qualified ROS 2 node name.
data Node = Node
    { nodePackage :: String
    , nodeName    :: String
    }
  deriving Generic

instance ToJSON Node

-- | Data that may be relevant to generate a ROS application.
data AppData = AppData
  { variables        :: [VarDecl]
  , monitors         :: [Monitor]
  , copilot          :: Maybe Command.Standalone.AppData
  , testingApps      :: [Node]
  , testingVariables :: [VarDecl]
  }
  deriving (Generic)

instance ToJSON AppData

-- | Name of the function to be used to generate random values of a given type.
randomBaseType :: String -- ^ Type to generate random values of.
               -> String
randomBaseType ty = case ty of
  "bool"     -> "randomBool"
  "uint8_t"  -> "randomInt"
  "uint16_t" -> "randomInt"
  "uint32_t" -> "randomInt"
  "uint64_t" -> "randomInt"
  "int8_t"   -> "randomInt"
  "int16_t"  -> "randomInt"
  "int32_t"  -> "randomInt"
  "int64_t"  -> "randomInt"
  "float"    -> "randomFloat"
  "double"   -> "randomFloat"
  def        -> def
