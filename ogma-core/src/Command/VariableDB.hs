{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
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
-- | Variable DBs.
module Command.VariableDB
    ( VariableDB(..)
    , InputDef(..)
    , Connection(..)
    , TopicDef(..)
    , TypeDef(..)
    , emptyVariableDB
    , findInput
    , findConnection
    , findTopic
    , findType
    , mergeVariableDB
    )
  where

-- External imports
import Control.Monad.Except (ExceptT, throwError)
import Data.Aeson           (FromJSON (..))
import Data.Aeson.TH        (defaultOptions, deriveFromJSON, fieldLabelModifier)
import Data.Char            (toLower)
import Data.List            (find)
import Data.Maybe           (isNothing)
import GHC.Generics         (Generic)

-- External imports: auxiliary
import Data.List.Extra (toHead)
import Data.Location (Location(..))

-- Internal imports
import Command.Errors (ErrorTriplet(..), ErrorCode)

-- * Variable Databases

-- | A variable database.
data VariableDB = VariableDB
    { inputs  :: [InputDef]
    , topics  :: [TopicDef]
    , types   :: [TypeDef]
    }
  deriving (Generic, Show)

-- | Definition of an input variable.
data InputDef = InputDef
    { inputName        :: String
    , inputType        :: Maybe String
    , inputConnections :: [ Connection ]
    }
  deriving (Eq, Show)

-- | Definition of a connection to a topic.
data Connection = Connection
    { connectionScope :: String
    , connectionTopic :: String
    , connectionField :: Maybe String
    }
  deriving (Eq, Show)

-- | Definition of a topic.
data TopicDef = TopicDef
    { topicScope :: String
    , topicTopic :: String
    , topicType  :: String
    }
  deriving (Eq, Show)

-- | Definition of a type or type mapping.
data TypeDef = TypeDef
    { typeFromScope :: String
    , typeFromType  :: String
    , typeFromField :: Maybe String
    , typeToScope   :: String
    , typeToType    :: String
    }
  deriving (Eq, Show)

-- | A variable database with no entries.
emptyVariableDB :: VariableDB
emptyVariableDB = VariableDB [] [] []

-- | Find an input with a given name.
findInput :: VariableDB -> String -> Maybe InputDef
findInput varDB name =
  find (\x -> inputName x == name) (inputs varDB)

-- | Find a connection a given scope.
findConnection :: InputDef -> String -> Maybe Connection
findConnection inputDef scope =
  find (\x -> connectionScope x == scope) (inputConnections inputDef)

-- | Find a topic a given scope and name.
findTopic :: VariableDB -> String -> String -> Maybe TopicDef
findTopic varDB scope name =
  find (\x -> topicScope x == scope && topicTopic x == name) (topics varDB)

-- | Find a type with a given input name, scope, and destination system.
findType :: VariableDB -> String -> String -> String -> Maybe TypeDef
findType varDB name scope destConn = do
  inputDef <- findInput varDB name
  let connectionDef :: Maybe Connection
      connectionDef = findConnection inputDef scope

      field :: Maybe String
      field = connectionField =<< connectionDef

      topic :: Maybe String
      topic = connectionTopic <$> connectionDef

      topicDef :: Maybe TopicDef
      topicDef = findTopic varDB scope =<< topic

      ty :: Maybe String
      ty = topicType <$> topicDef

  let match :: TypeDef -> Bool
      match typeDef =
        case (inputType inputDef, ty) of
          (Just ty1, Nothing) ->
                  typeFromScope typeDef == scope
               && typeFromField typeDef == field
               && typeToScope typeDef   == destConn
               && typeToType typeDef    == ty1
          (Just ty1, Just ty2) ->
                  typeFromScope typeDef == scope
               && typeFromType typeDef  == ty2
               && typeFromField typeDef == field
               && typeToScope typeDef   == destConn
               && typeToType typeDef    == ty1
          (_ ,      Just ty2) ->
                  typeFromScope typeDef == scope
               && typeFromType typeDef  == ty2
               && typeFromField typeDef == field
               && typeToScope typeDef   == destConn
          (Nothing, Nothing) -> False

  find match (types varDB)

-- ** Merging of variable DBs

-- | Merge two variable DBs, so long as they do not contain contradictory
-- information.
mergeVariableDB :: Monad m
                => VariableDB -> VariableDB -> ExceptT ErrorTriplet m VariableDB
mergeVariableDB varDB1 varDB2 = do
  inputs' <- mergeInputs (inputs varDB1) (inputs varDB2)
  topics' <- mergeTopics (topics varDB1) (topics varDB2)
  types'  <- mergeTypes  (types varDB1)  (types varDB2)
  return $ VariableDB inputs' topics' types'

-- | Merge two lists of input definitions, so long as they do not contain
-- contradictory information.
mergeInputs :: Monad m
            => [InputDef] -> [InputDef] -> ExceptT ErrorTriplet m [InputDef]
mergeInputs is1 []     = return is1
mergeInputs is1 (i2:is2) = do
  is1' <- mergeInput is1 i2
  mergeInputs is1' is2

-- | Merge an input definition into a list of input definitions, so long as it
-- does not contain contradictory information.
mergeInput :: Monad m
           => [InputDef] -> InputDef -> ExceptT ErrorTriplet m [InputDef]
mergeInput []       i2 = return [i2]
mergeInput (i1:is1) i2
  | inputName i1 == inputName i2
    && (  isNothing (inputType i1)
       || isNothing (inputType i2)
       || inputType i1 == inputType i2
       )
  = do cs <- mergeConnections (inputConnections i1) (inputConnections i2)
       let i1' = i1 { inputType =
                        mergeMaybe (inputType i1) (inputType i2)
                    , inputConnections =
                        cs
                    }
       return (i1' : is1)

  | otherwise
  = do is1' <- mergeInput is1 i2
       return $ i1 : is1'

-- | Merge two lists of connections, so long as they do not contain
-- contradictory information.
mergeConnections :: Monad m
                 => [Connection] -> [Connection] -> ExceptT ErrorTriplet m [Connection]
mergeConnections cs1 []       = return cs1
mergeConnections cs1 (c2:cs2) = do
  cs1' <- mergeConnection cs1 c2
  mergeConnections cs1' cs2

-- | Merge a connection into a list of connections, so long as it does not
-- contain contradictory information.
mergeConnection ::  Monad m
                => [Connection] -> Connection -> ExceptT ErrorTriplet m [Connection]
mergeConnection []       c2 = return [c2]
mergeConnection (c1:cs1) c2
  | c1 == c2
  = return $ c1 : cs1

  | connectionScope c1 == connectionScope c2
  = throwError $
      cannotMergeVariableDBs "connections with the same scopes"

  | otherwise
  = do cs1' <- mergeConnection cs1 c2
       return (c1 : cs1')

-- | Merge two lists of topics, so long as they do not contain contradictory
-- information.
mergeTopics :: Monad m
            => [TopicDef] -> [TopicDef] -> ExceptT ErrorTriplet m [TopicDef]
mergeTopics ts1 [] = return ts1
mergeTopics ts1 (t2:ts2) = do
  ts1' <- mergeTopic ts1 t2
  mergeTopics ts1' ts2

-- | Merge a topic into a list of topics, so long as it does not contain
-- contradictory information.
mergeTopic :: Monad m
           => [TopicDef] -> TopicDef -> ExceptT ErrorTriplet m [TopicDef]
mergeTopic []       t2 = return [t2]
mergeTopic (t1:ts1) t2
  | t1 == t2
  = return $ t1 : ts1

  | topicScope t1 == topicScope t2 && topicTopic t1 == topicTopic t2
  = throwError $
      cannotMergeVariableDBs "topics with the same scopes and different types"

  | otherwise
  = do ts1' <- mergeTopic ts1 t2
       return (t1 : ts1')

-- | Merge two lists of type definitions, so long as they do not contain
-- contradictory information.
mergeTypes :: Monad m
           => [TypeDef] -> [TypeDef] -> ExceptT ErrorTriplet m [TypeDef]
mergeTypes ts1 []       = return ts1
mergeTypes ts1 (t2:ts2) = do
  ts1' <- mergeType ts1 t2
  mergeTypes ts1' ts2

-- | Merge a type definition into a list of type definitions, so long as it
-- does not contain contradictory information.
mergeType :: Monad m
           => [TypeDef] -> TypeDef -> ExceptT ErrorTriplet m [TypeDef]
mergeType []       t2 = return [t2]
mergeType (t1:ts1) t2
  | t1 == t2
  = return $ t1 : ts1

  |    typeFromScope t1 == typeFromScope t2
    && typeFromType t1  == typeFromType t2
    && typeToScope t1   == typeToScope t2
  = throwError $
      cannotMergeVariableDBs
        "types with the same scopes and from types but otherwise different"

  | otherwise
  = do ts1' <- mergeType ts1 t2
       return (t1 : ts1')

-- | Exception handler to deal with the case of variable DB files that cannot
-- be merged due to having incompatible information.
cannotMergeVariableDBs :: String -> ErrorTriplet
cannotMergeVariableDBs element =
    ErrorTriplet ecCannotMergeVariableDB msg LocationNothing
  where
    msg =
      "Reading variable DBs has failed due to them having incompatible"
      ++ " information for " ++ element ++ "."

-- | Error: one of the variable DBs provided cannot be merged.
ecCannotMergeVariableDB :: ErrorCode
ecCannotMergeVariableDB = 1

-- | Merge two @Maybe@ values, prefering the left one if two @Just@s are
-- provided.
mergeMaybe :: Maybe a -> Maybe a -> Maybe a
mergeMaybe Nothing x       = x
mergeMaybe x       Nothing = x
mergeMaybe x       _       = x

-- | Implement default instances of parser to read variable DB from JSON,
-- dropping the prefix in each field name.
deriveFromJSON
  defaultOptions {fieldLabelModifier = toHead toLower . drop 4 }
  ''TypeDef

deriveFromJSON
  defaultOptions {fieldLabelModifier = toHead toLower . drop 5 }
  ''TopicDef

deriveFromJSON
  defaultOptions {fieldLabelModifier = toHead toLower . drop 10 }
  ''Connection

deriveFromJSON
  defaultOptions {fieldLabelModifier = toHead toLower . drop 5 }
  ''InputDef

instance FromJSON VariableDB
