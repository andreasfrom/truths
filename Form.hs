{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Form (showForm, Form(..), Assignments, Formula, (+++)) where

import qualified Data.Foldable as F
import qualified Data.Text     as T

type Assignments = [(T.Text, Bool)]
type Formula = Form T.Text

data Form a = Atom a
            | Not (Form a)
            | And (Form a) (Form a)
            | Or (Form a) (Form a)
            | Xor (Form a) (Form a)
            | If (Form a) (Form a)
            | Iff (Form a) (Form a)
            deriving (Functor, F.Foldable)

instance Show (Formula) where
  show = T.unpack . showForm

infixr 5 +++
(+++) :: T.Text -> T.Text -> T.Text
(+++) = T.append

showForm :: Formula -> T.Text
showForm (Atom p) = p
showForm (Not (Atom p)) = "¬" +++ p
showForm (Not p) = "¬(" +++ showForm p +++ ")"
showForm o@(And p q) = parens (grade o) (grade p) "∧" (grade q)
showForm o@(Or p q)  = parens (grade o) (grade p) "∨" (grade q)
showForm o@(Xor p q) = parens (grade o) (grade p) "⊕" (grade q)
showForm o@(If p q)  = parens (grade o) (grade p) " ⇒ " (grade q)
showForm o@(Iff p q) = parens (grade o) (grade p) " ⇔ " (grade q)

data Assoc = Associative | AssocLeft | AssocRight
           deriving (Show, Eq)

data Precedence a = Precedence (Form a) Int Int Assoc

grade :: Form a -> Precedence a
grade p@(Atom _)  = Precedence p 0 0 Associative
grade p@(Not _)   = Precedence p 1 1 Associative
grade p@(And _ _) = Precedence p 2 2 Associative
grade p@(Or _ _)  = Precedence p 2 3 Associative
grade p@(Xor _ _) = Precedence p 2 4 Associative
grade p@(If _ _)  = Precedence p 3 5 AssocRight
grade p@(Iff _ _) = Precedence p 3 6 Associative

parens :: Precedence T.Text -> Precedence T.Text -> T.Text -> Precedence T.Text -> T.Text
parens (Precedence _ ol ot oa) (Precedence p pl pt _) op (Precedence q ql qt _)
  | ot == pt && oa == Associative = (if ql < ol then plains else rights) p op q
  | ot == qt && oa == Associative = (if pl < ol then plains else lefts) p op q
  | ot == pt && ql < ol && oa == AssocLeft = lefts p op q
  | ot == qt && pl < ol && oa == AssocRight = rights p op q
  | pl < ol && ql < ol = plains p op q
  | pl < ol = rights p op q
  | ql < ol = lefts p op q
  | otherwise = boths p op q

plains, lefts, rights, boths :: Formula -> T.Text -> Formula -> T.Text
plains p op q = showForm p +++ op +++ showForm q
lefts p op q = "(" +++ showForm p +++ ")" +++ op +++ showForm q
rights p op q = showForm p +++ op +++ "(" +++ showForm q +++ ")"
boths p op q = "(" +++ showForm p +++ ")" +++ op +++ "(" +++ showForm q +++ ")"
