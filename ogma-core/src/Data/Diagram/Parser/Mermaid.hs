{-# LANGUAGE OverloadedStrings #-}
-- Copyright 2024 United States Government as represented by the Administrator
-- of the National Aeronautics and Space Administration. All Rights Reserved.
--
-- Disclaimers
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at
--
--      https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.
--
-- | Parsing of diagrams from Mermaid files.
module Data.Diagram.Parser.Mermaid
    ( parseDiagramMermaid
    )
  where

-- External imports
import           Control.Monad              (void, when)
import           Data.ByteString.Lazy       (toStrict)
import qualified Data.ByteString.Lazy       as B
import           Data.Either                (isLeft)
import           Data.Functor.Identity      (Identity)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (ErrorFancy (ErrorFail), ParsecT,
                                             choice, empty, errorBundlePretty,
                                             fancyFailure, many, manyTill,
                                             noneOf, parse, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, digitChar,
                                             newline, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- Internal imports: auxiliary
import Data.Diagram  (Diagram (..))
import Data.ExprPair (ExprPair (..), ExprPairT (..), exprPairShow)

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

-- | Parser for mermaid diagrams.
pDiagram :: ExprPair -> MermaidParser Diagram
pDiagram  exprP =
      pGraphDiagram exprP
  <|> pStateDiagram exprP
  <|> pSequenceDiagram exprP

-- | Parser for a mermaid diagram.
--
-- This parser depends on an auxiliary parser for the expressions associated to
-- the edges or connections between states.
pGraphDiagram :: ExprPair -> MermaidParser Diagram
pGraphDiagram exprP = do
  _ <- string "graph" <* spaces
  _name <- T.pack <$> manyTill alphaNumChar (char ';')
  _ <- newline

  transitions <- many (pGraphTransition exprP)

  pure $ Diagram transitions

{-# ANN pGraphTransition ("HLint: ignore Redundant bracket" :: String) #-}
-- | Parser for an edge in a state diagram.
--
-- This parser depends on an auxiliary parser for the expressions associated to
-- the edges or connections between states.
pGraphTransition :: ExprPair -> MermaidParser (Int, String, Int)
pGraphTransition ep@(ExprPair (ExprPairT { exprTParse = parseProp })) = do
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

-- | Parser for Mermaid diagrams of type stateDiagram-v2.
pStateDiagram :: ExprPair -> MermaidParser Diagram
pStateDiagram exprPair = do
  _ <- string "stateDiagram-v2" <* spaces

  transitions <- many (pStateTransition exprPair)

  pure $ Diagram transitions

{-# ANN pStateTransition ("HLint: ignore Redundant bracket" :: String)  #-}
{-# ANN pStateTransition ("HLint: ignore Reduce duplication" :: String) #-}
-- | Parser for transition label in stateDiagram-v2 mermaid diagram.
pStateTransition :: ExprPair -> MermaidParser (Int, String, Int)
pStateTransition ep@(ExprPair (ExprPairT { exprTParse = parseProp })) = do
  _ <- spaces
  from <- read <$> many digitChar
  _ <- spaces
  string "-->"
  _ <- spaces
  to <- read <$> many digitChar
  _ <- spaces
  _ <- char ':'
  _ <- spaces
  edge <- many (noneOf ("\n" :: [Char]))

  let x = parseProp edge
  when (isLeft x) $ fancyFailure $ Set.singleton $
    ErrorFail $ "Edge property has incorrect format: " ++ show edge

  _ <- newline

  pure (from, exprPairShow ep edge, to)

-- | Parser for Mermaid diagrams of type sequenceDiagram.
pSequenceDiagram :: ExprPair -> MermaidParser Diagram
pSequenceDiagram exprPair = do
  spaces
  _ <- string "sequenceDiagram"
  spaces

  conditions <- many (pSequenceTransition exprPair)
  let transitions = zipWith (\t idx -> (idx, t, idx + 1)) conditions [0..]

  pure $ Diagram transitions

{-# ANN pSequenceTransition ("HLint: ignore Redundant bracket" :: String) #-}
-- | Parser for a connection, message or transition in a sequence diagram.
--
-- This parser depends on an auxiliary parser for the expressions associated to
-- the connections or messages between elements.
pSequenceTransition :: ExprPair -> MermaidParser String
pSequenceTransition ep@(ExprPair (ExprPairT { exprTParse = parseProp })) = do
  spaces
  stateFrom <- many digitChar
  spaces
  pSequenceArrow
  spaces
  stateTo <- many digitChar
  spaces
  _ <- char ':'
  spaces
  edge <- many (noneOf ("\n" :: [Char]))

  let x = parseProp edge
  when (isLeft x) $ fancyFailure $ Set.singleton $
    ErrorFail $ "Edge property has incorrect format: " ++ show edge

  _ <- newline

  pure (exprPairShow ep edge)

-- | Parser for arrow in sequence diagram.
pSequenceArrow :: MermaidParser ()
pSequenceArrow = void $ choice
  [ string "->>"
  , string "-->>"
  , string "-)"
  ]

-- | Consume spaces.
spaces :: MermaidParser ()
spaces = L.space space1 empty empty
