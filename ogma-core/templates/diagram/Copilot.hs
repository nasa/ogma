import           Copilot.Compile.C99
import           Copilot.Language         hiding (max, min, prop)
import           Copilot.Language.Prelude
import           Copilot.Library.LTL      (next)
import           Copilot.Library.MTL      hiding (alwaysBeen, since, trigger)
import qualified Copilot.Library.MTL      as MTL
import           Copilot.Library.PTLTL    (alwaysBeen, previous, since)
import qualified Copilot.Library.PTLTL    as PTLTL
import           Language.Copilot         (reify)
import           Language.Copilot         hiding (max, min)
import           Prelude                  hiding (max, min, mod, not, until,
                                           (&&), (++), (/=), (<), (<=), (==),
                                           (>), (>=), (||))

externalState :: Stream Word8
externalState = extern "{{{state}}}" Nothing

input :: Stream Word8
input = extern "{{{input}}}" Nothing

{{{streamDefs}}}

-- | Complete specification. Calls C handler functions when properties are
-- violated.
spec :: Spec
spec = do
  trigger "handler" stateMachineProp {{{handlerInputs}}}

main :: IO ()
main = reify spec >>= compile "{{{specName}}}"

-- Initial state, final state, no transition signal, transitions, bad state
type StateMachineGF = ( Word8, Word8, Stream Bool, [(Word8, Stream Bool, Word8)], Word8)

stateMachineGF :: StateMachineGF -> Stream Word8
stateMachineGF (initialState, finalState, noInputData, transitions, badState) = state
  where
    state = [initialState] ++ ifThenElses transitions

    ifThenElses :: [(Word8, Stream Bool, Word8)] -> Stream Word8
    ifThenElses [] =
      ifThenElse (state == constant finalState && noInputData)
        (constant finalState)
        (constant badState)

    ifThenElses ((s1,i,s2):ss) =
      ifThenElse (state == constant s1 && i) (constant s2) (ifThenElses ss)

-- | True when the given input stream does hold any of the values in the given
-- list.
noneOf :: [Stream Bool] -> Stream Bool
noneOf []     = true
noneOf (x:xs) = not x && noneOf xs

-- | Given a list of transitions, and a current state, and a list of possible
-- destination states, produce a list of booleans indicating if a transition to
-- each of the destination states would be valid.
checkValidTransitions :: [(Word8, Stream Bool, Word8)]
                      -> Stream Word8
                      -> [Word8]
                      -> [Stream Bool]
checkValidTransitions transitions curState destinations =
  map (checkValidTransition transitions curState) destinations

-- | Given a list of transitions, and a current state, and destination states,
-- produce a list of booleans indicating if a transition to each of the
-- destination states would be valid.
checkValidTransition :: [(Word8, Stream Bool, Word8)]
                     -> Stream Word8
                     -> Word8
                     -> Stream Bool
checkValidTransition []                 _   _   = true
checkValidTransition ((so1, c, sd1):sx) so2 sd2 =
  ifThenElse
    ((constant so1 == so2) && (constant sd1 == constant sd2))
    c
    (checkValidTransition sx so2 sd2)
