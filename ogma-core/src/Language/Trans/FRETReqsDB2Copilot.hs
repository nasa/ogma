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

-- | Transform a FRET temporal logic specification contained in a requirements
-- database into a Copilot specification.
--
-- Ideally, this module would be implemented as a conversion between ASTs, but
-- we want to add comments to the generated code, which are not representable
-- in the abstract syntax tree.
module Language.Trans.FRETReqsDB2Copilot
    ( FRETReqsDB2CopilotOptions(..)
    , fret2CopilotModule
    )
  where

-- External imports
import Data.List ( nub, sort )

-- Internal imports: SMV
import qualified Language.SMV.AbsSMV        as SMV ( BoolSpec (..) )
import qualified Language.Trans.SMV2Copilot as SMV ( boolSpec2Copilot,
                                                     boolSpecNames )

-- Internal imports: CoCoSpec
import qualified Language.CoCoSpec.AbsCoCoSpec   as CoCoSpec ( BoolSpec (..) )
import qualified Language.Trans.CoCoSpec2Copilot as CoCoSpec ( boolSpec2Copilot,
                                                               boolSpecNames )

import qualified Language.FRETReqsDB.AST as FRET ( FRETReqsDB, semantics,
                                                   semanticsCoCoSpec,
                                                   semanticsFretish )

-- | Options used to customize the conversion of FRET Component Specifications
-- to Copilot code.
data FRETReqsDB2CopilotOptions = FRETReqsDB2CopilotOptions
  { fretReqsDB2CopilotUseCoCoSpec :: Bool
  , fretReqsDB2CopilotFilename    :: String
  }

-- | Return a string with the contents of the Copilot module that implements a
-- CoCoSpec BoolSpec.
--
-- PRE: The BoolSpec does not use any identifiers that exist in Copilot,
-- or any of @prop@, @clock@, @ftp@, @ot@, @pre@. All identifiers used are
-- valid C99 identifiers.
fret2CopilotModule :: FRETReqsDB2CopilotOptions
                   -> FRET.FRETReqsDB
                   -> Either String String
fret2CopilotModule prefs x = do
  cocoSpec <- FRET.semanticsCoCoSpec $ FRET.semantics x
  smvSpec  <- FRET.semanticsFretish  $ FRET.semantics x
  pure $ fret2CopilotModule' prefs smvSpec cocoSpec

fret2CopilotModule' :: FRETReqsDB2CopilotOptions
                    -> SMV.BoolSpec
                    -> CoCoSpec.BoolSpec
                    -> String
fret2CopilotModule' prefs smvSpec cocoSpec = unlines $ concat sections
  where
    specS = if fretReqsDB2CopilotUseCoCoSpec prefs
              then CoCoSpec.boolSpec2Copilot cocoSpec
              else SMV.boolSpec2Copilot smvSpec

    idents   = nub $ sort $ if fretReqsDB2CopilotUseCoCoSpec prefs
                              then CoCoSpec.boolSpecNames cocoSpec
                              else SMV.boolSpecNames smvSpec

    sections | fretReqsDB2CopilotUseCoCoSpec prefs
             = [ imports, propDef, externs, clock, ftp, undef, tpre, spec
               , main'
               ]

             | otherwise
             = [ imports, propDef, externs, clock, ftp, undef, tpre, spec
               , main'
               ]

    imports :: [String]
    imports =
      [ "import           Copilot.Compile.C99"
      , "import           Copilot.Language          hiding (prop)"
      , "import           Copilot.Language.Prelude"
      , "import           Copilot.Library.LTL       (next)"
      , "import           Copilot.Library.MTL       hiding (since,"
        ++ " alwaysBeen, trigger)"
      , "import           Copilot.Library.PTLTL     (since, previous,"
        ++ " alwaysBeen)"
      , "import qualified Copilot.Library.MTL       as MTL"
      , "import qualified Copilot.Library.PTLTL     as PTLTL"
      , "import           Language.Copilot          (reify)"
      , "import Prelude                   hiding ((&&), (||), (++), (<=), (>=),"
        ++ " (<), (>), (==), (/=), not)"
      , ""
      ]

    propDef  = [ ""
               , "-- | Property to monitor."
               , "prop :: Stream Bool"
               , "prop = " ++ specS
               , ""
               ]

    externs  = map (\i -> i ++ " = extern " ++ show i ++ " Nothing") idents

    clock    = [ ""
               , "-- | Clock that increases in one-unit steps."
               , "clock :: Stream Int64"
               , "clock = [0] ++ (clock + 1)"
               , ""
               ]

    ftp      = [ ""
               , "-- | First Time Point"
               , "ftp :: Stream Bool"
               , "ftp = [True] ++ false"
               , ""
               ]

    undef    = [ "ot :: Int -> Int -> Stream Bool -> Stream Bool"
               , "ot = undefined"
               , "pre :: Stream Bool -> Stream Bool"
               , "pre = ([False] ++)"
               , "tpre :: Stream Bool -> Stream Bool"
               , "tpre = ([True] ++)"
               ]

    tpre     = [ ""
               , "tpre :: Stream Bool -> Stream Bool"
               , "tpre = ([True] ++)"
               ]

    spec     = [ ""
               , "-- | Complete specification. Calls the C function void handler(); when"
               , "-- the property is violated."
               , "spec :: Spec"
               , "spec = do"
               , "  trigger  \"handler\" prop []"
               , ""
               ]

    main'    = [ ""
               , "main :: IO ()"
               , "main = reify spec >>= compile \""
                    ++ fretReqsDB2CopilotFilename prefs ++ "\""
               ]
