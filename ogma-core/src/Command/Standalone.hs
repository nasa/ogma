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
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY
-- OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT
-- LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO
-- SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
-- PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE
-- SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF
-- PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN
-- ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR
-- RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR
-- ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE. FURTHER,
-- GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING
-- THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES
-- IT "AS IS."
--
-- Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST
-- THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS
-- ANY PRIOR RECIPIENT. IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN
-- ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE,
-- INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S
-- USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE
-- UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY
-- PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW. RECIPIENT'S SOLE REMEDY
-- FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS
-- AGREEMENT.
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
