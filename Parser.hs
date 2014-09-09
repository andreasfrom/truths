{-# LANGUAGE OverloadedStrings #-}

module Parser(parseInput, Input(..)) where

import           Control.Applicative    hiding (many, (<|>))
import qualified Data.Text              as T
import           Form
import           Text.Parsec.Combinator (choice, eof)
import           Text.Parsec.Error      (ParseError)
import           Text.Parsec.Expr
import           Text.Parsec.Prim
import           Text.Parsec.Text
import           Text.Parser.Char

data Input = Single Formula | Equality Formula Formula deriving (Show)

input :: Parser Input
input = try (Equality <$> expr <*> (spaces *> char '=' *> spaces *> expr) <* eof)
        <|> (Single <$> expr <* eof)

expr :: Parser Formula
expr = buildExpressionParser table term

term :: Parser Formula
term = (char '(' *> spaces *> expr <* char ')' <* spaces)
       <|> (Atom <$> T.pack <$> (some (oneOf (['a'..'u'] ++ ['w'..'z']))) <* spaces)

-- And, Or, Xor and Iff are associative so it doesn't matter which way they associate,
-- If associates to the right so p => q => r is interpreted as p => (q => r)
table = [ [ prefix "¬!" Not ]
        , [ binary "^&∧" And AssocLeft
          , binary "v∨" Or AssocLeft
          , binary "+⊕" Xor AssocLeft
          ]
        , [ composite ["=>", "->", "⇒"] If AssocRight
          , composite ["<=>", "<->", "⇔"] Iff AssocLeft
          ]
        ]
  where prefix s f = Prefix (oneOf s *> spaces *> pure f)
        binary s f = Infix (oneOf s *> spaces *> pure f)
        composite s f = Infix (choice (map (try . string) s) *> spaces *> pure f)

parseInput :: T.Text -> Either ParseError (Input)
parseInput = parse input ""
