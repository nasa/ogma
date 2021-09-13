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
{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Functor law"   -}

-- | Representation and parser of FRET Component Specifications.
--
-- FRET files are JSON files, implemented in Haskell using type classes, so the
-- parser is defined in the same module as the AST to avoid having orphan
-- instances.
module Language.FRETComponentSpec.AST where

-- External imports
import           Data.Aeson          ( FromJSON (..), Value (Object), (.:) )
import           Data.Aeson.Types    ( prependFailure, typeMismatch )
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

-- Internal imports
import qualified Language.CoCoSpec.AbsCoCoSpec as CoCoSpec
import qualified Language.CoCoSpec.ParCoCoSpec as CoCoSpec ( myLexer,
                                                             pBoolSpec )

import qualified Language.SMV.AbsSMV   as SMV
import qualified Language.SMV.ParSMV   as SMV ( myLexer, pBoolSpec )

-- | Abstract representation of a FRET file.
data FRETComponentSpec = FRETComponentSpec
    { fretName              :: String
    , fretInternalVariables :: [ FRETInternalVariableDef ]
    , fretExternalVariables :: [ FRETExternalVariableDef ]
    , fretRequirements      :: [ FRETRequirement ]
    }
  deriving (Show)

-- | Instance to parse FRET semantics keys in JSON format.
instance FromJSON FRETComponentSpec where
  parseJSON (Object v)
      | (specName, Object specValues) <- head (M.toList v)
      = FRETComponentSpec (T.unpack specName)
      <$> specValues .: "Internal_variables"
      <*> specValues .: "Other_variables"
      <*> specValues .: "Requirements"

      | (specName, specValues) <- head (M.toList v)
      = prependFailure "parsing FRET Component Specification failed, "
          (typeMismatch "Object" specValues)

  parseJSON invalid =
    prependFailure "parsing FRET Component Specification failed, "
      (typeMismatch "Object" invalid)

-- | Internal variable definition, with a given name, its type and either a
-- Lustre or a Copilot expression.
data FRETInternalVariableDef = FRETInternalVariableDef
    { fretInternalVariableName    :: String
    , fretInternalVariableType    :: String
    , fretInternalVariableLustre  :: String
    , fretInternalVariableCopilot :: String
    }
  deriving (Show)

instance FromJSON FRETInternalVariableDef where
  parseJSON (Object v) = FRETInternalVariableDef
    <$> v .: "name"
    <*> v .: "type"
    <*> v .: "assignmentLustre"
    <*> v .: "assignmentCopilot"

  parseJSON invalid =
    prependFailure "parsing FRET Internal Variable definition failed, "
      (typeMismatch "Object" invalid)

-- | External variable definition, with a given name and type.
--
-- The value of external variables is assigned outside Copilot, so they have no
-- defining expression in this type..
data FRETExternalVariableDef = FRETExternalVariableDef
    { fretExternalVariableName :: String
    , fretExternalVariableType :: String
    }
  deriving (Show)

instance FromJSON FRETExternalVariableDef where
  parseJSON (Object v) = FRETExternalVariableDef
    <$> v .: "name"
    <*> v .: "type"

  parseJSON invalid =
    prependFailure "parsing FRET External Variable failed, "
      (typeMismatch "Object" invalid)

-- | Requirement with a given name and a CoCoSpec expression.
data FRETRequirement = FRETRequirement
    { fretRequirementName       :: String
    , fretRequirementCoCoSpec   :: Maybe (Either String CoCoSpec.BoolSpec)
    , fretRequirementPTExpanded :: Maybe (Either String SMV.BoolSpec)
    , fretRequirementFretish    :: String
    }
  deriving (Show)

instance FromJSON FRETRequirement where
  parseJSON (Object v) = FRETRequirement
    <$> v .: "name"
    <*> (fmap (CoCoSpec.pBoolSpec . CoCoSpec.myLexer) <$> v .: "CoCoSpecCode")
    <*> (fmap (SMV.pBoolSpec . SMV.myLexer) <$> v .: "ptLTL")
    <*> (v .: "fretish")

  parseJSON invalid =
    prependFailure "parsing FRET Requirement failed, "
      (typeMismatch "Object" invalid)

-- | Apply a variable subsitution to variables and requirements in a FRET
-- file.
applySubstitution :: (String, String) -> FRETComponentSpec -> FRETComponentSpec
applySubstitution sub file =
    FRETComponentSpec tlName
                      tlInternalVariables
                      tlExternalVariables
                      tlReqs
  where

    -- Result component spec fields
    tlName              = fretName file
    tlInternalVariables = map internalVarMapF $ fretInternalVariables file
    tlExternalVariables = map externalVarMapF $ fretExternalVariables file
    tlReqs              = map reqMapF $ fretRequirements file

    -- Mapping function for fields with names to substitute
    internalVarMapF x = x { fretInternalVariableName =
                              subsName sub (fretInternalVariableName x) }

    externalVarMapF x = x { fretExternalVariableName =
                              subsName sub (fretExternalVariableName x)}

    reqMapF x = x { fretRequirementName =
                      subsName sub (fretRequirementName x)

                  , fretRequirementPTExpanded =
                      fmap (fmap (subBS sub)) (fretRequirementPTExpanded x)
                  }

    -- Substitute name x if it matches the old name oName
    subsName (oName, nName) x = if x == oName then nName else x

    -- Substitute a name in all identifiers in a boolean expression
    subBS sub' = mapBoolSpecIdent (subsName sub')

    -- Traverse a boolean expression applying a function to all identifiers
    mapBoolSpecIdent :: (String -> String) -> SMV.BoolSpec -> SMV.BoolSpec
    mapBoolSpecIdent f boolSpec =
      case boolSpec of
        SMV.BoolSpecPar bs -> SMV.BoolSpecPar (mapBoolSpecIdent f bs)

        SMV.BoolSpecBPar bs -> SMV.BoolSpecBPar (mapBoolSpecIdent f bs)

        SMV.BoolSpecIPar bs -> SMV.BoolSpecIPar (mapBoolSpecIdent f bs)

        SMV.BoolSpecConst bc -> SMV.BoolSpecConst bc

        SMV.BoolSpecSignal (SMV.Ident i) -> SMV.BoolSpecSignal (SMV.Ident (f i))

        SMV.BoolSpecOp1 op spec -> SMV.BoolSpecOp1 op (mapBoolSpecIdent f spec)

        SMV.BoolSpecOp2 spec1 op2 spec2 -> SMV.BoolSpecOp2
                                             (mapBoolSpecIdent f spec1) op2
                                             (mapBoolSpecIdent f spec2)
