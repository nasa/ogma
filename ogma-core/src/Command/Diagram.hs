{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
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
-- | Transform a state diagram into a Copilot specification.
module Command.Diagram
    ( diagram
    , DiagramOptions(..)
    , DiagramFormat(..)
    , DiagramMode(..)
    , DiagramPropFormat(..)
    , ErrorCode
    )
  where

-- External imports
import Control.Exception    as E
import Control.Monad.Except (runExceptT)
import Data.Aeson           (object, (.=))
import Data.Char            (toLower)
import Data.Foldable        (for_)
import Data.Text.Lazy       (pack)
import System.FilePath      ((</>))

-- External imports: auxiliary
import System.Directory.Extra ( copyTemplate )

-- Internal imports: auxiliary
import           Command.Errors      (ErrorTriplet (..))
import           Command.Result      (Result (..))
import           Data.Diagram.Parser (DiagramFormat (..), readDiagram)
import           Data.ExprPair       (ExprPair (..), ExprPairT (..))
import qualified Data.ExprPair
import           Data.Location       (Location (..))
import           Paths_ogma_core     (getDataDir)

-- Internal imports: language ASTs, transformers
import Language.Trans.Diagram2Copilot (DiagramMode (..), diagram2CopilotSpec)

-- | Generate a new Copilot monitor that implements a state machine described
-- in a diagram given as an input file.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @stateMachine@, @externalState@, @main@, @spec@,
-- @stateMachine1@, @clock@, @ftp@, @notPreviousNot@. All identifiers used are
-- valid C99 identifiers. The template, if provided, exists and uses the
-- variables needed by the diagram application generator. The target directory
-- is writable and there's enough disk space to copy the files over.
diagram :: FilePath       -- ^ Path to a file containing a diagram
        -> DiagramOptions -- ^ Customization options
        -> IO (Result ErrorCode)
diagram fp options = do
  E.handle (return . diagramTemplateError fp) $ do
    -- Sub-parser for edge expressions.
    let functions = exprPair (diagramPropFormat options)

    -- Convert the diagram into elements in a Copilot spec.
    copilotSpecElems <- diagram' fp options functions

    -- Convert the elements into a success or error result.
    let (mOutput, result) = diagramResult fp copilotSpecElems

    -- If the result is success, expand the template.
    for_ mOutput $ \(streamDefs, triggers) -> do
      let subst = object
                    [ "streamDefs" .= pack streamDefs
                    , "specName"   .= pack (diagramFilename options)
                    , "input"      .= pack (diagramInputVar options)
                    , "state"      .= pack (diagramStateVar options)
                    , "triggers"   .= pack triggers
                    ]

      templateDir <- case diagramTemplateDir options of
                       Just x  -> return x
                       Nothing -> do
                         dataDir <- getDataDir
                         return $ dataDir </> "templates" </> "diagram"

      let targetDir = diagramTargetDir options

      copyTemplate templateDir subst targetDir

    return result

-- | Generate a new Copilot monitor that implements a state machine described
-- in a diagram given as an input file, using a subexpression handler.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @stateMachine@, @externalState@, @main@, @spec@,
-- @stateMachine1@, @clock@, @ftp@, @notPreviousNot@. All identifiers used are
-- valid C99 identifiers. The template, if provided, exists and uses the
-- variables needed by the diagram application generator. The target directory
-- is writable and there's enough disk space to copy the files over.
diagram' :: FilePath
         -> DiagramOptions
         -> ExprPair
         -> IO (Either String (String, String))
diagram' fp options exprP = do
  diagramE <- runExceptT $ readDiagram fp (diagramFormat options) exprP
  case diagramE of
    Left (ErrorTriplet _ec msg _loc) -> pure $ Left msg
    Right diagramR ->
      pure $ Right $ diagram2CopilotSpec diagramR (diagramMode options)

-- | Options used to customize the conversion of diagrams to Copilot code.
data DiagramOptions = DiagramOptions
  { diagramTargetDir   :: FilePath
  , diagramTemplateDir :: Maybe FilePath
  , diagramFormat      :: DiagramFormat
  , diagramPropFormat  :: DiagramPropFormat
  , diagramFilename    :: String
  , diagramMode        :: DiagramMode
  , diagramStateVar    :: String
  , diagramInputVar    :: String
  }

-- | Property formats supported.
data DiagramPropFormat = Lustre
                       | Inputs
                       | Literal
                       | SMV
  deriving (Eq, Show)

-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error code used is 1 for user error.
type ErrorCode = Int

-- | Error: the input file cannot be read due to it being unreadable or the
-- format being incorrect.
ecDiagramError :: ErrorCode
ecDiagramError = 1

-- | Error: diagram component generation failed during the copy/write
-- process.
ecDiagramTemplateError :: ErrorCode
ecDiagramTemplateError = 2

-- * Result

-- | Process the result of the transformation function.
diagramResult :: FilePath
              -> Either String a
              -> (Maybe a, Result ErrorCode)
diagramResult fp result = case result of
  Left msg -> (Nothing, Error ecDiagramError msg (LocationFile fp))
  Right t  -> (Just t,  Success)

-- | Report an error when trying to open or copy the template.
diagramTemplateError :: FilePath
                     -> E.SomeException
                     -> Result ErrorCode
diagramTemplateError fp exception =
    Error ecDiagramTemplateError msg (LocationFile fp)
  where
    msg =
      "Diagram monitor generation failed during copy/write operation. Check"
      ++ " that there's free space in the disk and that you have the necessary"
      ++ " permissions to write in the destination directory. "
      ++ show exception

-- * Handler for boolean expressions in edges or transitions between states.

-- | Return a handler depending on the format used for edge or transition
-- properties.
exprPair :: DiagramPropFormat -> ExprPair
exprPair Inputs = ExprPair $
  ExprPairT
    ((Right . read) :: String -> Either String Int)
    (const id)
    (\x -> "input == " ++ show x)
    (const [])
    (-1)
exprPair f = Data.ExprPair.exprPair (formatName f)
  where
    formatName = map toLower . show
