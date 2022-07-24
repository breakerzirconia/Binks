-- Copyright (c) 2022 Constantine Ter-Matevosian
--
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Eso.Binks.Internal.Parser where

import           Data.Functor
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Eso.Binks.Core

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

{- Grammar for 'Link's

s ::= e
    | e e
e ::= 0
    | 1
    | '(' e e ')'

-}

link :: Parser Link
link = e >>= \l -> ((l :><:) <$> e) <|> return l
  where
    e :: Parser Link
    e = (single '0' $> zero) <|>
        (single '1' $> one) <|>
        (single '(' *> ((:><:) <$> e <*> e) <* single ')')
