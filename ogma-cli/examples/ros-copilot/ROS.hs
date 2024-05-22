import Copilot.Compile.C99
import Copilot.Language
import Language.Copilot    (reify)
import Prelude             hiding (not, (&&), (>=))

inputSignal :: Stream Int64
inputSignal = extern "input_signal" Nothing

inputSignalFloat :: Stream Float
inputSignalFloat = extern "input_signal_float" Nothing

inputSignalDouble :: Stream Double
inputSignalDouble = extern "input_signal_double" Nothing

propTestCopilot :: Stream Bool
propTestCopilot = inputSignal >= 5
               && inputSignalFloat >= 5
               && inputSignalDouble >= 5

spec :: Spec
spec = do
  trigger "handlerTestCopilot" (not propTestCopilot) []

main :: IO ()
main = reify spec >>= compileWith settings "monitor"
  where
    settings = mkDefaultCSettings
                 { cSettingsOutputDirectory = "demo/copilot/src/" }
