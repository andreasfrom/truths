{-# LANGUAGE OverloadedStrings #-}

module Parser(parseInput, Input(..)) where

import           Control.Applicative    hiding (many, (<|>))
import qualified Data.Text              as T
import           Form                   hiding (AssocLeft, AssocRight)
import           Semtab
import           Text.Parsec.Combinator (choice, eof)
import           Text.Parsec.Error      (ParseError)
import           Text.Parsec.Expr
import           Text.Parsec.Prim
import           Text.Parsec.Text
import           Text.Parser.Char

data Input = Single Formula
           | Equivalence Formula Formula
           | Context Formula
           | Latex Formula
           | Tableau FormTab
           deriving (Show)

input :: Parser Input
input = try (Equivalence <$> expr <*> (spaces *> char '=' *> spaces *> expr) <* eof)
        <|> try (Context <$> (string "context" *> spaces *> expr <* eof))
        <|> try (Latex <$> (string "latex" *> spaces *> expr <* eof))
        <|> try (Tableau <$> (Case (Just 1) <$> (string "tab" *> optional (string "leau") *> spaces *> expr)
                              <*> (toBool <$> (choice (map string ["T", "t", "1", "F", "f", "0"])))))
        <|> (Single <$> expr <* eof)
  where toBool x | x == "T" || x == "t" || x == "1" = True
                 | otherwise = False

expr :: Parser Formula
expr = buildExpressionParser table term

term :: Parser Formula
term = (char '(' *> spaces *> expr <* char ')' <* spaces)
       <|> (Atom <$> T.pack <$> (some (oneOf (['a'..'u'] ++ ['w'..'z']))) <* spaces)

table = [ [ prefix "¬!" Not ]
        , [ binary "^&∧" And AssocLeft ]
        , [ binary "v∨" Or AssocLeft ]
        , [ binary "+⊕" Xor AssocLeft ]
        , [ composite ["=>", "->", "⇒"] If AssocRight ]
        , [ composite ["<=>", "<->", "⇔"] Iff AssocLeft ]
        ]
  where prefix s f = Prefix (oneOf s *> spaces *> pure f)
        binary s f = Infix (oneOf s *> spaces *> pure f)
        composite s f = Infix (choice (map (try . string) s) *> spaces *> pure f)

parseInput :: T.Text -> Either ParseError (Input)
parseInput = parse input ""
