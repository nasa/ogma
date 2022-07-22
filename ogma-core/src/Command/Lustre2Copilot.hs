-- | Transform a Lustre file into a Copilot specification.
module Command.Lustre2Copilot
    ( lustre2Copilot )
  where

-- External imports
import System.Exit ( ExitCode (ExitFailure), exitWith )
import System.IO   ( hPutStrLn, stderr )

-- Internal imports: Auxiliary
import Data.String.Extra as S ( safeReadFile )

-- Internal imports: Lustre parsing and AST
import qualified Language.Lustre.AbsLustre as L ( Program )
import qualified Language.Lustre.ParLustre as L ( myLexer, pProgram )

-- Internal imports: Transformation of Lustre to Copilot
import qualified Language.Trans.Lustre2Copilot as L ( lustre2Copilot )

-- | Print the contents of a Copilot module that implements monitors for
-- the units in a Lustre file.
--
-- PRE: The file given is readable, contains a valid Lustre file., none of the
-- variables used in Lustre clash with any identifiers that exist in Copilot.
-- All identifiers used are valid C99 identifiers.
lustre2Copilot :: FilePath -> IO ()
lustre2Copilot fp = do

  -- All of the following operations use Either to return error messages. The
  -- use of the monadic bind to pass arguments from one function to the next
  -- will cause the program to stop at the earliest error.
  --
  -- The guard (checking whether the result is Left or Right) must happen
  -- before the first side effects (output file creation), or the file may be
  -- (over) written even when processing fails.
  lustre <- parseLustreProgram fp
  case L.lustre2Copilot =<< lustre of
    Left msg -> do hPutStrLn stderr ("ogma: " ++ fp ++ ": error: " ++ msg)
                   exitWith (ExitFailure 1)
    Right t  -> putStrLn t

-- | Parse a Lustre file.
--
-- Returns a 'Left' with an error message if the file does not have the correct
-- format.
--
-- Throws an exception if the file cannot be read.
parseLustreProgram :: FilePath -> IO (Either String L.Program)
parseLustreProgram fp = do
  content <- S.safeReadFile fp
  return $ L.pProgram . L.myLexer =<< content
