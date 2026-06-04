{-# LANGUAGE ExistentialQuantification #-}
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
-- | Shared functions across multiple backends.
module Command.Common
    ( InputFile(..)
    , combineInputFiles
    , parseInputFile
    , parseVariablesFile
    , parseRequirementsListFile
    , openVarDBFiles
    , openVarDBFilesWithDefault
    , parseTemplateVarsFile
    , checkArguments
    , specExtractExternalVariables
    , specExtractHandlers
    , processResult
    , cannotCopyTemplate
    , makeLeftE
    , locateTemplateDir
    )
  where

-- External imports
import qualified Control.Exception      as E
import           Control.Monad.Except   (ExceptT (..), runExceptT, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (Value (Null, Object), eitherDecode,
                                         eitherDecodeFileStrict, object)
import           System.FilePath        ((</>))

-- External imports: auxiliary
import Data.ByteString.Extra   as B (safeReadFile)
import Data.String.Extra       (sanitizeLCIdentifier, sanitizeUCIdentifier)
import System.Directory.Extra  (CopyTemplateError (..))

-- External imports: ogma
import Data.OgmaSpec (Requirement (..), Spec (..), externalVariableName,
                      externalVariables, requirementName, requirementResultType,
                      requirements)

-- Internal imports: VariableDBs
import Command.VariableDB (VariableDB, emptyVariableDB, mergeVariableDB)

-- Internal imports: auxiliary
import Command.Errors      (ErrorTriplet(..), ErrorCode)
import Command.Result      (Result (..))
import Data.Diagram        (Diagram)
import Data.Diagram.Parser (DiagramFormat (..), readDiagram)
import Data.Either.Extra   (makeLeft)
import Data.ExprPair       (ExprPair (..), ExprPairT (..))
import Data.Location       (Location (..))
import Data.Spec.Parser    (readInputFile)
import Paths_ogma_core     (getDataDir)

-- | File containing information to be processed by Ogma (e.g., specification,
-- diagram).
data InputFile a = InputFileDiagram Diagram
                 | InputFileSpec    (Spec a)

-- | Merge a list of input files into a smaller list of input files.
--
-- PRE: If there is more than one input file, all input files are specs.
combineInputFiles :: [InputFile a] -> [InputFile a]
combineInputFiles []  = []
combineInputFiles [x] = [x]
combineInputFiles xs
    | any isInputDiagram xs
    = []

    | otherwise
    = [ InputFileSpec $ foldr1 mergeSpecs $ map getSpec xs ]

  where

    -- True if the given argument is a diagram.
    isInputDiagram :: InputFile a -> Bool
    isInputDiagram (InputFileDiagram _) = True
    isInputDiagram _                    = False

    -- Merge two specifications.
    mergeSpecs :: Spec a -> Spec a -> Spec a
    mergeSpecs s1 s2 = Spec
      { internalVariables = internalVariables s1 ++ internalVariables s2
      , externalVariables = externalVariables s2 ++ externalVariables s2
      , requirements      = requirements s1 ++ requirements s2
      }

    -- Unsafely unwrap the spec in an input file.
    --
    -- PRE: The argument input file contains a spec.
    getSpec :: InputFile a -> Spec a
    getSpec (InputFileSpec s) = s
    getSpec _                 = error "The input file provided is not a spec"

-- | Process input file, it contains a valid diagram or specification, and
-- return its abstract representation.
parseInputFile :: FilePath
               -> String
               -> String
               -> Maybe String
               -> ExprPairT a
               -> ExceptT ErrorTriplet IO (InputFile a)
parseInputFile fp formatName propFormatName propVia exprT
    | isDiagramFormat formatName
    = InputFileDiagram <$>
        readDiagram fp diagramFormat (ExprPair exprT)

    | otherwise
    = InputFileSpec <$>
        readInputFile fp formatName propFormatName propVia exprT

  where

    isDiagramFormat :: String -> Bool
    isDiagramFormat fName = fName `elem` [ "dot", "mermaid" ]

    diagramFormat :: DiagramFormat
    diagramFormat
      | formatName == "dot"     = Dot
      | formatName == "mermaid" = Mermaid
      | otherwise               = error $
         "diagramFormat: Not a diagram format " ++ show formatName

-- | Process a variable selection file, if available, and return the variable
-- names.
parseVariablesFile :: Maybe FilePath
                   -> ExceptT ErrorTriplet IO (Maybe [String])
parseVariablesFile Nothing   = return Nothing
parseVariablesFile (Just fp) = do
  -- Fail if the file cannot be opened.
  varNamesE <- liftIO $ E.try $ lines <$> readFile fp
  case (varNamesE :: Either E.SomeException [String]) of
    Left _         -> throwError $ cannotOpenVarFile fp
    Right varNames -> return $ Just varNames

-- | Process a requirements / handlers list file, if available, and return the
-- handler names.
parseRequirementsListFile :: Maybe FilePath
                          -> ExceptT ErrorTriplet IO (Maybe [String])
parseRequirementsListFile Nothing   = return Nothing
parseRequirementsListFile (Just fp) =
  ExceptT $ makeLeftE (cannotOpenHandlersFile fp) <$>
    (E.try $ Just . lines <$> readFile fp)

-- | Read a list of variable DBs.
openVarDBFiles :: VariableDB
               -> [FilePath]
               -> ExceptT ErrorTriplet IO VariableDB
openVarDBFiles acc []     = return acc
openVarDBFiles acc (x:xs) = do
    file <- parseVarDBFile (Just x)
    acc' <- mergeVariableDB acc file
    openVarDBFiles acc' xs

  where

    -- Process a variable database file, if available.
    parseVarDBFile :: Maybe FilePath
                   -> ExceptT ErrorTriplet IO VariableDB
    parseVarDBFile Nothing   = return emptyVariableDB
    parseVarDBFile (Just fn) =
      ExceptT $ makeLeft (cannotOpenDB fn) <$>
        eitherDecodeFileStrict fn

-- | Read a list of variable DBs, as well as the default variable DB.
openVarDBFilesWithDefault :: [FilePath]
                          -> ExceptT ErrorTriplet IO VariableDB
openVarDBFilesWithDefault files = do
  dataDir <- liftIO getDataDir
  let defaultDB = dataDir </> "data" </> "variable-db.json"
  openVarDBFiles emptyVariableDB (files ++ [defaultDB])

-- | Process a JSON file with additional template variables to make available
-- during template expansion.
parseTemplateVarsFile :: Maybe FilePath
                      -> ExceptT ErrorTriplet IO Value
parseTemplateVarsFile Nothing   = return $ object []
parseTemplateVarsFile (Just fp) = do
  content <- liftIO $ B.safeReadFile fp
  let value = eitherDecode =<< content
  case value of
    Right x@(Object _) -> return x
    Right x@Null       -> return x
    Right _            -> throwError (cannotReadObjectTemplateVars fp)
    _                  -> throwError (cannotOpenTemplateVars fp)

-- | Check that the arguments provided are sufficient to operate.
--
-- The backend provides several modes of operation, which are selected
-- by providing different arguments to the `ros` command.
--
-- When an input specification file is provided, the variables and requirements
-- defined in it are used unless variables or handlers files are provided, in
-- which case the latter take priority.
--
-- If an input file is not provided, then the user must provide BOTH a variable
-- list, and a list of handlers.
checkArguments :: Maybe (InputFile a)
               -> Maybe [String]
               -> Maybe [String]
               -> Either ErrorTriplet ()
checkArguments Nothing Nothing   Nothing   = Left wrongArguments
checkArguments Nothing Nothing   _         = Left wrongArguments
checkArguments Nothing _         Nothing   = Left wrongArguments
checkArguments _       (Just []) _         = Left wrongArguments
checkArguments _       _         (Just []) = Left wrongArguments
checkArguments _       _         _         = Right ()

-- | Extract the variables from a specification, and sanitize them.
specExtractExternalVariables :: Maybe (Spec a) -> [String]
specExtractExternalVariables Nothing   = []
specExtractExternalVariables (Just cs) = map sanitizeLCIdentifier
                                       $ map externalVariableName
                                       $ externalVariables cs

-- | Extract the requirements from a specification, and sanitize them to match
-- the names of the handlers used by Copilot.
specExtractHandlers :: Maybe (Spec a) -> [(String, Maybe String)]
specExtractHandlers Nothing   = []
specExtractHandlers (Just cs) = map extractReqData
                              $ requirements cs
  where
    extractReqData r =
      (handlerNameF (requirementName r), requirementResultType r)

    handlerNameF = ("handler" ++) . sanitizeUCIdentifier

-- * Errors

-- | Process a computation that can fail with an error code, and turn it into a
-- computation that returns a 'Result'.
processResult :: Monad m => ExceptT ErrorTriplet m a -> m (Result ErrorCode)
processResult m = do
  r <- runExceptT m
  case r of
    Left (ErrorTriplet errorCode msg location)
      -> return $ Error errorCode msg location
    _ -> return Success

-- ** Error messages

-- | Exception handler to deal with the case in which the arguments
-- provided are incorrect.
wrongArguments :: ErrorTriplet
wrongArguments =
    ErrorTriplet ecWrongArguments msg LocationNothing
  where
    msg =
      "the arguments provided are insufficient: you must provide an input "
      ++ "specification, or both a variables and a handlers file."

-- | Exception handler to deal with the case in which the variable DB cannot be
-- opened.
cannotOpenDB :: FilePath -> ErrorTriplet
cannotOpenDB file =
    ErrorTriplet ecCannotOpenDBFile msg (LocationFile file)
  where
    msg =
      "cannot open variable DB file " ++ file

-- | Exception handler to deal with the case in which the variable file
-- provided by the user cannot be opened.
cannotOpenVarFile :: FilePath -> ErrorTriplet
cannotOpenVarFile file =
    ErrorTriplet ecCannotOpenVarFile  msg (LocationFile file)
  where
    msg =
      "cannot open variable list file " ++ file

-- | Exception handler to deal with the case in which the handlers file cannot
-- be opened.
cannotOpenHandlersFile :: FilePath -> ErrorTriplet
cannotOpenHandlersFile file =
    ErrorTriplet ecCannotOpenHandlersFile msg (LocationFile file)
  where
    msg =
      "cannot open handlers file " ++ file

-- | Exception handler to deal with the case in which the template vars file
-- cannot be opened.
cannotOpenTemplateVars :: FilePath -> ErrorTriplet
cannotOpenTemplateVars file =
    ErrorTriplet ecCannotOpenTemplateVarsFile msg (LocationFile file)
  where
    msg =
      "Cannot open file with additional template variables: " ++ file

-- | Exception handler to deal with the case in which the template vars file
-- cannot be opened.
cannotReadObjectTemplateVars :: FilePath -> ErrorTriplet
cannotReadObjectTemplateVars file =
    ErrorTriplet ecCannotReadObjectTemplateVarsFile msg (LocationFile file)
  where
    msg =
      "Cannot open file with additional template variables: " ++ file

-- | Exception handler to deal with the case of files that cannot be
-- copied/generated due to a problem with the template or the target
-- location.
cannotCopyTemplate :: E.SomeException -> ErrorTriplet
cannotCopyTemplate e =
    ErrorTriplet ecCannotCopyTemplate msg location
  where
    (msg, location) = case E.fromException e of
      Just (CopyTemplateListError dir reason) ->
        ( "Generation failed because the template directory " ++ dir
          ++ " cannot be read: " ++ reason
        , LocationFile dir
        )

      Just (CopyTemplateReadError file reason) ->
        ( "Generation failed because the template file " ++ file
          ++ " cannot be read: " ++ reason
        , LocationFile file
        )

      Just (CopyTemplateDecodeError file reason) ->
        ( "Generation failed because the template file " ++ file
          ++ " is not a valid UTF-8 text file: " ++ reason
        , LocationFile file
        )

      Just (CopyTemplateParseError file reason) ->
        ( "Generation failed because the template file " ++ file
          ++ " cannot be parsed: " ++ reason
        , LocationFile file
        )

      Just (CopyTemplateWriteError file reason) ->
        ( "Generation failed because the file " ++ file
          ++ " cannot be written. Check that there's free space in the disk"
          ++ " and that you have the necessary permissions to write in the"
          ++ " destination directory: " ++ reason
        , LocationFile file
        )

      Nothing ->
        ( "Generation failed during copy/write operation. Check that"
          ++ " there's free space in the disk and that you have the necessary"
          ++ " permissions to write in the destination directory: " ++ show e
        , LocationNothing
        )

-- ** Error codes

-- | Error: wrong arguments provided.
ecWrongArguments :: ErrorCode
ecWrongArguments = 1

-- | Error: the variable DB provided by the user cannot be opened.
ecCannotOpenDBFile :: ErrorCode
ecCannotOpenDBFile = 1

-- | Error: the variable file provided by the user cannot be opened.
ecCannotOpenVarFile :: ErrorCode
ecCannotOpenVarFile = 1

-- | Error: the handlers file provided by the user cannot be opened.
ecCannotOpenHandlersFile :: ErrorCode
ecCannotOpenHandlersFile = 1

-- | Error: the template vars file provided by the user cannot be opened.
ecCannotOpenTemplateVarsFile :: ErrorCode
ecCannotOpenTemplateVarsFile = 1

-- | Error: the template variables file passed does not contain a JSON object.
ecCannotReadObjectTemplateVarsFile :: ErrorCode
ecCannotReadObjectTemplateVarsFile = 1

-- | Error: the files cannot be copied/generated due to a problem with the
-- template or the target location.
ecCannotCopyTemplate :: ErrorCode
ecCannotCopyTemplate = 1

-- * Auxiliary Functions

-- | Return the path to the template directory.
locateTemplateDir :: Maybe FilePath
                  -> FilePath
                  -> ExceptT e IO FilePath
locateTemplateDir mTemplateDir name =
  case mTemplateDir of
    Just x  -> return x
    Nothing -> liftIO $ do
      dataDir <- getDataDir
      return $ dataDir </> "templates" </> name

-- | Replace the left Exception in an Either.
makeLeftE :: c -> Either E.SomeException b -> Either c b
makeLeftE = makeLeft
