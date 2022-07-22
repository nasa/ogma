-- | Custom Setup that runs bnfc to generate the language sub-libraries
-- for the parsers included in Ogma.
module Main (main) where

import Distribution.Simple         ( defaultMainWithHooks, hookedPrograms,
                                     postConf, preBuild, simpleUserHooks )
import Distribution.Simple.Program ( Program (..), findProgramVersion,
                                     simpleProgram )
import System.Process              ( system )

-- | Run BNFC on the grammar before the actual build step.
--
-- All options for bnfc are hard-coded here. There is an open bug in Cabal's
-- github repo about supporting BNFC.
main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
  { hookedPrograms = [ bnfcProgram ]
  , postConf       = \args flags packageDesc localBuildInfo -> do
      _ <- system "bnfc --force --haskell -p Language.Copilot -o src/ grammar/Copilot.cf"
      postConf simpleUserHooks args flags packageDesc localBuildInfo
  }

-- | TODO: This should be in Cabal.Distribution.Simple.Program.Builtin.
bnfcProgram :: Program
bnfcProgram = (simpleProgram "bnfc")
  { programFindVersion = findProgramVersion "--version" id
  }
