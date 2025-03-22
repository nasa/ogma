{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
-- Copyright 2024 United States Government as represented by the Administrator
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
-- | Transform a state diagram into a Copilot specification.
module Command.Diagram
    ( diagram
    , DiagramOptions(..)
    , DiagramFormat(..)
    , DiagramMode(..)
    , DiagramPropFormat(..)
    , ErrorCode
    )
  where

-- External imports
import           Control.Exception                 as E
import           Control.Monad                     (when)
import           Data.Aeson                        (object, (.=))
import           Data.ByteString.Lazy              (toStrict)
import qualified Data.ByteString.Lazy              as B
import           Data.Either                       (isLeft)
import           Data.Foldable                     (for_)
import           Data.Functor.Identity             (Identity)
import           Data.GraphViz                     (graphEdges)
import qualified Data.GraphViz                     as G
import qualified Data.GraphViz.Attributes.Complete as Attributes
import           Data.GraphViz.Commands.IO         (toUTF8)
import qualified Data.GraphViz.Parsing             as G
import           Data.GraphViz.PreProcessing       (preProcess)
import qualified Data.GraphViz.Types.Generalised   as Gs
import           Data.List                         (intercalate, nub, sort)
import qualified Data.Set                          as Set
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import           Data.Text.Lazy                    (pack)
import qualified Data.Text.Lazy                    as LT
import           Data.Void                         (Void)
import           System.FilePath                   ((</>))
import           Text.Megaparsec                   (ErrorFancy (ErrorFail),
                                                    ParsecT, empty,
                                                    errorBundlePretty,
                                                    fancyFailure, many,
                                                    manyTill, noneOf, parse)
import           Text.Megaparsec.Char              (alphaNumChar, char,
                                                    digitChar, newline, space1,
                                                    string)
import qualified Text.Megaparsec.Char.Lexer        as L


-- External imports: auxiliary
import Data.ByteString.Extra  as B ( safeReadFile )
import System.Directory.Extra ( copyTemplate )

-- External imports: parsing expressions.
import qualified Language.Lustre.ParLustre as Lustre (myLexer, pBoolSpec)
import qualified Language.SMV.ParSMV       as SMV (myLexer, pBoolSpec)

-- Internal imports: auxiliary
import Command.Result  (Result (..))
import Data.Location   (Location (..))
import Paths_ogma_core (getDataDir)

-- Internal imports: language ASTs, transformers
import           Language.SMV.Substitution     (substituteBoolExpr)
import qualified Language.Trans.Lustre2Copilot as Lustre (boolSpec2Copilot,
                                                          boolSpecNames)
import           Language.Trans.SMV2Copilot    as SMV (boolSpec2Copilot,
                                                       boolSpecNames)

-- | Generate a new Copilot monitor that implements a state machine described
-- in a diagram given as an input file.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @stateMachine@, @externalState@, @noneOf@,
-- @checkValidTransitions@, @main@, @spec@, @stateMachine1@, @clock@, @ftp@,
-- @notPreviousNot@. All identifiers used are valid C99 identifiers. The
-- template, if provided, exists and uses the variables needed by the diagram
-- application generator. The target directory is writable and there's enough
-- disk space to copy the files over.
diagram :: FilePath       -- ^ Path to a file containing a diagram
        -> DiagramOptions -- ^ Customization options
        -> IO (Result ErrorCode)
diagram fp options = do
  E.handle (return . diagramTemplateError fp) $ do
    -- Sub-parser for edge expressions.
    let functions = exprPair (diagramPropFormat options)

    -- Convert the diagram into elements in a Copilot spec.
    copilotSpecElems <- diagram' fp options functions

    -- Convert the elements into a success or error result.
    let (mOutput, result) = diagramResult fp copilotSpecElems

    -- If the result is success, expand the template.
    for_ mOutput $ \(streamDefs, handlerInputs) -> do
      let subst = object
                    [ "streamDefs"    .= pack streamDefs
                    , "specName"      .= pack (diagramFilename options)
                    , "input"         .= pack (diagramInputVar options)
                    , "state"         .= pack (diagramStateVar options)
                    , "handlerInputs" .= pack handlerInputs
                    ]

      templateDir <- case diagramTemplateDir options of
                       Just x  -> return x
                       Nothing -> do
                         dataDir <- getDataDir
                         return $ dataDir </> "templates" </> "diagram"

      let targetDir = diagramTargetDir options

      copyTemplate templateDir subst targetDir

    return result

-- | Generate a new Copilot monitor that implements a state machine described
-- in a diagram given as an input file, using a subexpression handler.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @stateMachine@, @externalState@, @noneOf@,
-- @checkValidTransitions@, @main@, @spec@, @stateMachine1@, @clock@, @ftp@,
-- @notPreviousNot@. All identifiers used are valid C99 identifiers. The
-- template, if provided, exists and uses the variables needed by the diagram
-- application generator. The target directory is writable and there's enough
-- disk space to copy the files over.
diagram' :: FilePath
         -> DiagramOptions
         -> ExprPair
         -> IO (Either String (String, String))
diagram' fp options exprP = do
  contentEither <- B.safeReadFile fp
  return $ do
    -- All of the following operations use Either to return error messages. The
    -- use of the monadic bind to pass arguments from one function to the next
    -- will cause the program to stop at the earliest error.
    diagFileContent <- contentEither

    -- Abtract representation of a state machine diagram.
    diagramR <- parseDiagram (diagramFormat options) diagFileContent exprP

    return $ diagramToCopilot diagramR (diagramMode options)

-- | Options used to customize the conversion of diagrams to Copilot code.
data DiagramOptions = DiagramOptions
  { diagramTargetDir   :: FilePath
  , diagramTemplateDir :: Maybe FilePath
  , diagramFormat      :: DiagramFormat
  , diagramPropFormat  :: DiagramPropFormat
  , diagramFilename    :: String
  , diagramMode        :: DiagramMode
  , diagramStateVar    :: String
  , diagramInputVar    :: String
  }

-- | Modes of operation.
data DiagramMode = CheckState   -- ^ Check if given state matches expectation
                 | ComputeState -- ^ Compute expected state
                 | CheckMoves   -- ^ Check if transitioning to a state would be
                                --   possible.
  deriving (Eq, Show)

-- | Diagram formats supported.
data DiagramFormat = Mermaid
                   | Dot
  deriving (Eq, Show)

-- | Property formats supported.
data DiagramPropFormat = Lustre
                       | Inputs
                       | Literal
                       | SMV
  deriving (Eq, Show)

-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error code used is 1 for user error.
type ErrorCode = Int

-- | Error: the input file cannot be read due to it being unreadable or the
-- format being incorrect.
ecDiagramError :: ErrorCode
ecDiagramError = 1

-- | Error: diagram component generation failed during the copy/write
-- process.
ecDiagramTemplateError :: ErrorCode
ecDiagramTemplateError = 2

-- * Result

-- | Process the result of the transformation function.
diagramResult :: FilePath
              -> Either String a
              -> (Maybe a, Result ErrorCode)
diagramResult fp result = case result of
  Left msg -> (Nothing, Error ecDiagramError msg (LocationFile fp))
  Right t  -> (Just t,  Success)

-- | Report an error when trying to open or copy the template.
diagramTemplateError :: FilePath
                     -> E.SomeException
                     -> Result ErrorCode
diagramTemplateError fp exception =
    Error ecDiagramTemplateError msg (LocationFile fp)
  where
    msg =
      "Diagram monitor generation failed during copy/write operation. Check"
      ++ " that there's free space in the disk and that you have the necessary"
      ++ " permissions to write in the destination directory. "
      ++ show exception

-- * Handler for boolean expressions in edges or transitions between states.

-- | Handler for boolean expressions that knows how to parse them, replace
-- variables in them, and convert them to Copilot.
data ExprPair = forall a . ExprPair
  { _exprParse   :: String -> Either String a
  , _exprReplace :: [(String, String)] -> a -> a
  , _exprPrint   :: a -> String
  , _exprIdents  :: a -> [String]
  }

-- | Return a handler depending on the format used for edge or transition
-- properties.
exprPair :: DiagramPropFormat -> ExprPair
exprPair Lustre  = ExprPair (Lustre.pBoolSpec . Lustre.myLexer)
                            (\_ -> id)
                            Lustre.boolSpec2Copilot
                            Lustre.boolSpecNames
exprPair Inputs  = ExprPair ((Right . read) :: String -> Either String Int)
                            (\_ -> id)
                            (\x -> "input == " ++ show x)
                            (const [])
exprPair Literal = ExprPair Right
                            (\_ -> id)
                            id
                            (const [])
exprPair SMV     = ExprPair (SMV.pBoolSpec . SMV.myLexer)
                            substituteBoolExpr
                            SMV.boolSpec2Copilot
                            SMV.boolSpecNames

-- | Parse and print a value using an auxiliary Expression Pair.
--
-- Fails if the value has no valid parse.
exprPairShow :: ExprPair -> String -> String
exprPairShow (ExprPair parseProp _replace printProp _ids) =
  printProp . fromRight' . parseProp

-- * Diagrams

-- | Internal representation for diagrams.
newtype Diagram = Diagram
    { diagramTransitions :: [(Int, String, Int)]
    }
  deriving (Show, Eq)

-- * Diagram parsers

-- | Generic function to parse a diagram.
parseDiagram :: DiagramFormat          -- ^ Format of the input file
             -> B.ByteString           -- ^ Contents of the diagram
             -> ExprPair               -- ^ Subparser for conditions or edge
                                       -- expressions
             -> Either String Diagram
parseDiagram Dot     = parseDiagramDot
parseDiagram Mermaid = parseDiagramMermaid

-- ** Dot parser

-- | Parse a DOT / Graphviz diagram.
parseDiagramDot :: B.ByteString -> ExprPair -> Either String Diagram
parseDiagramDot contents exprP = do
    let contentsUTF8 = toUTF8 contents
    dg <- fst $ G.runParser G.parse $ preProcess contentsUTF8
    return $ makeDiagram dg
  where
    makeDiagram :: Gs.DotGraph LT.Text -> Diagram
    makeDiagram g = Diagram links
      where
        links = map edgeToLink (graphEdges g)

        edgeToLink edge =
            ( read (LT.unpack o)
            , exprPairShow exprP (LT.unpack e)
            , read (LT.unpack d)
            )
          where
            o = G.fromNode edge
            d = G.toNode edge
            e = getLabel (G.edgeAttributes edge)

            -- Extract the label from a list of attributes. If no label is
            -- found, it's assumed that the condition is the literal true.
            getLabel [] = "true"
            getLabel ((Attributes.Label (Attributes.StrLabel l)) : _) = l
            getLabel (_ : as) = getLabel as

-- ** Mermaid parser

-- | Parse a mermaid diagram.
parseDiagramMermaid :: B.ByteString -> ExprPair -> Either String Diagram
parseDiagramMermaid txtDia exprP =
    case parsingResult of
      Left e  -> Left (errorBundlePretty e)
      Right x -> Right x
  where
    txt           = T.decodeUtf8 (toStrict txtDia)
    parsingResult = parse (spaces *> pDiagram exprP) "<input>" txt

-- | Type for parser for memaid diagrams.
type MermaidParser = ParsecT Void Text Identity

-- | Parser for a mermaid diagram.
--
-- This parser depends on an auxiliary parser for the expressions associated to
-- the edges or connections between states.
pDiagram :: ExprPair -> MermaidParser Diagram
pDiagram exprP = do
  _ <- string "graph" <* spaces
  _name <- T.pack <$> manyTill alphaNumChar (char ';')
  _ <- newline

  transitions <- many (pTransition exprP)

  pure $ Diagram transitions

-- | Parser for an edge in a state diagram.
--
-- This parser depends on an auxiliary parser for the expressions associated to
-- the edges or connections between states.
pTransition :: ExprPair -> MermaidParser (Int, String, Int)
pTransition ep@(ExprPair { _exprParse = parseProp }) = do
  _ <- spaces
  stateFrom <- many digitChar
  _ <- string "-->|"
  edge <- many (noneOf ("|" :: [Char]))

  let x = parseProp edge
  when (isLeft x) $ fancyFailure $ Set.singleton $
    ErrorFail $ "Edge property has incorrect format: " ++ show edge

  _ <- char '|'
  stateTo <- many digitChar
  _ <- char ';'
  _ <- newline
  return (read stateFrom, exprPairShow ep edge, read stateTo)

-- | Consume spaces
spaces :: MermaidParser ()
spaces = L.space space1 empty empty

-- * Backend

-- | Convert the diagram into a set of Copilot definitions, and a list of
-- arguments for the top-level handler.
diagramToCopilot :: Diagram -> DiagramMode -> (String, String)
diagramToCopilot diag mode = (machine, arguments)
  where
    machine = unlines
      [ "stateMachineProp :: Stream Bool"
      , "stateMachineProp = " ++ propExpr
      , ""
      , "stateMachine1 :: Stream Word8"
      , "stateMachine1 = stateMachineGF (initialState, finalState, noInput, "
        ++ "transitions, badState)"
      , ""
      , "-- Check"
      , "initialState :: Word8"
      , "initialState = " ++ show initialState
      , ""
      , "-- Check"
      , "finalState :: Word8"
      , "finalState = " ++ show finalState
      , ""
      , "noInput :: Stream Bool"
      , "noInput = false"
      , ""
      , "badState :: Word8"
      , "badState = " ++ show badState
      , ""
      , "transitions = " ++ showTransitions
      ]

    -- Elements of the spec.
    propExpr     = case mode of
                     CheckState   -> "stateMachine1 == externalState"
                     ComputeState -> "true"
                     CheckMoves   -> "true"
    initialState = minimum states
    finalState   = maximum states
    badState     = maximum states + 1

    -- Arguments for the handler.
    arguments = "[ " ++ intercalate ", " (map ("arg " ++) argExprs) ++ " ]"

    argExprs = case mode of
      CheckState   -> [ "stateMachine1", "externalState", "input" ]
      ComputeState -> [ "stateMachine1", "externalState", "input" ]
      CheckMoves   -> map stateCheckExpr states

    stateCheckExpr stateId =
      "(checkValidTransition transitions externalState " ++ show stateId ++ ")"

    -- States and transitions from the diagram.
    transitions = diagramTransitions diag
    states      = nub $ sort $ concat [ [x, y] | (x, _, y) <- transitions ]

    showTransitions :: String
    showTransitions = "[" ++ showTransitions' transitions

    showTransitions' :: [(Int, String, Int)] -> String
    showTransitions' []         = "]"
    showTransitions' (x1:x2:xs) =
      showTransition x1 ++ ", " ++ showTransitions' (x2:xs)
    showTransitions' (x2:[])    = showTransition x2 ++ "]"

    showTransition :: (Int, String, Int) -> String
    showTransition (a, b, c) =
      "(" ++ show a ++ ", " ++ b ++ ", " ++ show c ++ ")"

-- * Auxiliary functions

-- | Unsafe fromRight. Fails if the value is a 'Left'.
fromRight' :: Either a b -> b
fromRight' (Right v) = v
fromRight' _         = error "fromRight' applied to Left value."
