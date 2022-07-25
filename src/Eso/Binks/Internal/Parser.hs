-- Copyright (c) 2022 Constantine Ter-Matevosian
--
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Eso.Binks.Internal.Parser where

import           Data.Functor
import           Data.Text
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Eso.Binks.Core

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

binksVal :: Parser BinksValue
binksVal =
  (BinksNumber <$> L.signed sc L.scientific) <|>
  (BinksUnit <$ chunk "()")


{- | Grammar for 'Link's

link ::= e
       | e ' ' e
e ::= node
    | '(' e ' ' e ')'
    | '(' e ')'
node ::= "[<" binksVal ">]"

-}
link :: Parser (Link (Either Text BinksValue))
link = e >>= \l -> ((l :><:) <$> (sc *> e)) <|> return l
  where
    e :: Parser (Link (Either Text BinksValue))
    e = choice
      [ chunk "[<" *> (Node . Right <$> binksVal) <* chunk ">]"
      , Node . Left <$> nonDiscardedidentifier
      , single '(' *> ((:><:) <$> e <*> (sc *> e)) <* single ')'
      , between (single '(') (single ')') e
      ]

identifier :: Parser Text
identifier = discardedIdentifier <|> nonDiscardedidentifier

-- | Discarded identifiers are denoted with a single underscore.
discardedIdentifier :: Parser Text
discardedIdentifier = singleton <$> single '_'

-- | Non-discarded identifiers always start with a lowercase letter and are followed
-- by an alphanumeric character, an underscore, a hyphen, or an apostrophe.
nonDiscardedidentifier :: Parser Text
nonDiscardedidentifier = (pack .) . (:) <$>
                         lowerChar <*>
                         many (alphaNumChar <|> oneOf @[] "_-'")

{- | Grammar for 'Go's

go ::= <
     | <*
     | >
     | >*

-}
go :: Parser Go
go = (single '<' *> ((single '*' $> Forever L) <|> pure (Once L))) <|>
     (single '>' *> ((single '*' $> Forever R) <|> pure (Once R)))

{- | Grammar for 'Instruction's

instruction ::= '.'
              | identifier ' ' link
              | identifier ' ' '~' identifier

-}
instruction :: Parser (Instruction (Either Text BinksValue))
instruction = choice
  [ single '.' $> End
  , chunk "out" *> sc *> (Out <$> identifier)
  , single '!' *> sc *> (Update <$> identifier
                                <*> (sc *> some go)
                                <*> (sc *> ((Left <$> identifier) <|>
                                            (Right <$> between (single '<')
                                                               (single '>')
                                                               binksVal))))
  , single '?' *> sc *> (Access <$> identifier <*> (sc *> some go))
  , try (Mirror <$> identifier <*> (sc *> single '~' *> identifier))
  , CreateLink <$> identifier <*> (sc *> link)
  ]
