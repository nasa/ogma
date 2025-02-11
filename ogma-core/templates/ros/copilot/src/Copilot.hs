{{#copilot}}
import           Copilot.Compile.C99
import           Copilot.Language          hiding (prop)
import           Copilot.Language.Prelude
import           Copilot.Library.LTL       (next)
import           Copilot.Library.MTL       hiding (since, alwaysBeen, trigger)
import           Copilot.Library.PTLTL     (since, previous, alwaysBeen)
import qualified Copilot.Library.PTLTL     as PTLTL
import qualified Copilot.Library.MTL       as MTL
import           Language.Copilot          (reify)
import           Prelude                   hiding ((&&), (||), (++), (<=), (>=), (<), (>), (==), (/=), not)

{{{copilot.externs}}}
{{{copilot.internals}}}
{{{copilot.reqs}}}

-- | Clock that increases in one-unit steps.
clock :: Stream Int64
clock = [0] ++ (clock + 1)

-- | First Time Point
ftp :: Stream Bool
ftp = [True] ++ false

pre :: Stream Bool -> Stream Bool
pre = ([False] ++)

tpre :: Stream Bool -> Stream Bool
tpre = ([True] ++)

notPreviousNot :: Stream Bool -> Stream Bool
notPreviousNot = not . PTLTL.previous . not

-- | Complete specification. Calls C handler functions when properties are
-- violated.
spec :: Spec
spec = do
{{{copilot.triggers}}}

main :: IO ()
main = reify spec >>= compile "{{{copilot.specName}}}"
{{/copilot}}
{{^copilot}}
-- No specification provided. Place your specification in this file.
{{/copilot}}
