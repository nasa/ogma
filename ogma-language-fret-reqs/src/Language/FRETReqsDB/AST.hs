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
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Functor law"   -}

-- | Implementation of a (partial) FRET requirements database parser and a
-- datatype representing FRET Requirements DB files.
--
-- FRET files are JSON files, which is implemented using type classes, so the
-- parser is defined in the same module as the AST to avoid having orphan
-- instances.
--
module Language.FRETReqsDB.AST where

-- External imports
import           Data.Aeson       ( FromJSON (..), Value (Object), withObject,
                                    (.:) )
import           Data.Aeson.Types ( prependFailure, typeMismatch )
import qualified Data.Text        as T
import           GHC.Generics     ( Generic )

-- Internal imports
import qualified Language.CoCoSpec.AbsCoCoSpec as CoCoSpec
import qualified Language.CoCoSpec.ParCoCoSpec as CoCoSpec ( myLexer,
                                                             pBoolSpec )

import qualified Language.SMV.AbsSMV as SMV
import qualified Language.SMV.ParSMV as SMV ( myLexer, pBoolSpec )

-- * FRETReqsDB file format

-- | Datatype representing FRETReqsDB files.
--
-- Only the fields we are interested in are supported.
data FRETReqsDB = FRETReqsDB
  { reqid        :: T.Text
  , parent_reqid :: T.Text
  , project      :: T.Text
  , rationale    :: T.Text
  , fulltext     :: T.Text
  , semantics    :: FRETSemantics
  }
  deriving (Generic, Show)

-- | Instance to parse FRETReqsDB files in JSON format.
instance FromJSON FRETReqsDB where
  parseJSON (Object v) = FRETReqsDB
    <$> v .: "reqid"
    <*> v .: "parent_reqid"
    <*> v .: "project"
    <*> v .: "rationale"
    <*> v .: "fulltext"
    <*> v .: "semantics"

  parseJSON invalid =
    prependFailure "parsing FRET Requirement Database failed, "
      (typeMismatch "Object" invalid)

-- | Datatype representing the semantics key of a FRET file.
--
-- Only the fields we are interested in are supported.
data FRETSemantics = FRETSemantics
  { semanticsType     :: T.Text
  , semanticsFretish  :: Either String SMV.BoolSpec
  , semanticsCoCoSpec :: Either String CoCoSpec.BoolSpec
  }
  deriving (Generic, Show)

-- | Instance to parse FRET semantics keys in JSON format.
instance FromJSON FRETSemantics where
  parseJSON = withObject "semantics" $ \v -> FRETSemantics
    <$> v .: "type"
    <*> (SMV.pBoolSpec <$> SMV.myLexer <$> T.unpack <$> v .: "ptExpanded")
    <*> (CoCoSpec.pBoolSpec
           <$> CoCoSpec.myLexer <$> T.unpack <$> v .: "CoCoSpecCode")
