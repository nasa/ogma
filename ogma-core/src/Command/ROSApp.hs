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
    , ErrorCode
    )
  where

-- External imports
import           Control.Applicative  (liftA2)
import qualified Control.Exception    as E
import           Control.Monad.Except (ExceptT (..), liftEither)
import           Data.Aeson           (ToJSON (..))
import           Data.List            (find)
import           Data.Maybe           (fromMaybe, mapMaybe)
import           GHC.Generics         (Generic)

-- External imports: auxiliary
import System.Directory.Extra (copyTemplate)

import qualified Command.Standalone

-- Internal imports: auxiliary
import Command.Result (Result (..))

-- Internal imports
import Command.Common

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
    varDB <- parseVarDBFile varDBFile

    spec  <- maybe (return Nothing) (\f -> Just <$> parseInputFile' f) fp

    liftEither $ checkArguments spec vs rs

    copilotM <- sequenceA $ liftA2 processSpec spec fp

    let varNames = fromMaybe (specExtractExternalVariables spec) vs
        monitors = fromMaybe (specExtractHandlers spec) rs

    let appData   = AppData variables monitors copilotM
        variables = mapMaybe (variableMap varDB) varNames

    return appData

  where

    fp             = commandInputFile options
    varNameFile    = commandVariables options
    varDBFile      = commandVariableDB options
    handlersFile   = commandHandlers options
    formatName     = commandFormat options
    propFormatName = commandPropFormat options
    propVia        = commandPropVia options

    parseInputFile' f =
      parseInputFile f formatName propFormatName propVia exprT

    processSpec spec' fp' =
      Command.Standalone.commandLogic fp' "copilot" [] exprT spec'

-- ** Argument processing

-- | Options used to customize the conversion of specifications to ROS
-- applications.
data CommandOptions = CommandOptions
  { commandInputFile   :: Maybe FilePath -- ^ Input specification file.
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
  }

-- | Return the variable information needed to generate declarations
-- and subscriptions for a given variable name and variable database.
--
-- We map the types to the specific types needed for the variable declaration
-- and the message subscription in ROS.
variableMap :: [(String, String, String, String)]
            -> String
            -> Maybe VarDecl
variableMap varDB varName =
    csvToVarMap <$> find (sameName varName) varDB

  where

    -- True if the given variable and db entry have the same name
    sameName :: String
             -> (String, String, String, String)
             -> Bool
    sameName n (vn, _, _, _) = n == vn

    -- Convert a DB row into Variable info needed to generate the ROS file
    csvToVarMap :: (String, String, String, String)
                -> VarDecl
    csvToVarMap (nm, ty, mid, mn) = VarDecl nm (typeVar ty) mid (typeMsg ty)

    typeVar ty = case ty of
      "uint8_t"  -> "std::uint8_t"
      "uint16_t" -> "std::uint16_t"
      "uint32_t" -> "std::uint32_t"
      "uint64_t" -> "std::uint64_t"
      "int8_t"   -> "std::int8_t"
      "int16_t"  -> "std::int16_t"
      "int32_t"  -> "std::int32_t"
      "int64_t"  -> "std::int64_t"
      "float"    -> "float"
      "double"   -> "double"
      def        -> def

    typeMsg ty = case ty of
      "bool"     -> "std_msgs::msg::Bool"
      "uint8_t"  -> "std_msgs::msg::UInt8"
      "uint16_t" -> "std_msgs::msg::UInt16"
      "uint32_t" -> "std_msgs::msg::UInt32"
      "uint64_t" -> "std_msgs::msg::UInt64"
      "int8_t"   -> "std_msgs::msg::Int8"
      "int16_t"  -> "std_msgs::msg::Int16"
      "int32_t"  -> "std_msgs::msg::Int32"
      "int64_t"  -> "std_msgs::msg::Int64"
      "float"    -> "std_msgs::msg::Float32"
      "double"   -> "std_msgs::msg::Float64"
      def        -> def

-- | The declaration of a variable in C, with a given type and name.
data VarDecl = VarDecl
    { varDeclName    :: String
    , varDeclType    :: String
    , varDeclId      :: String
    , varDeclMsgType :: String
    }
  deriving Generic

instance ToJSON VarDecl

-- | The name of a handler associated to each condition.
type Monitor = String

-- | Data that may be relevant to generate a ROS application.
data AppData = AppData
  { variables :: [VarDecl]
  , monitors  :: [Monitor]
  , copilot   :: Maybe Command.Standalone.AppData
  }
  deriving (Generic)

instance ToJSON AppData
