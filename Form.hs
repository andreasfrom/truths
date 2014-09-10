{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Form where

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
            deriving (Functor, F.Foldable, Eq, Ord)

instance Show (Formula) where
  show = T.unpack . showForm

infixr 5 +++
(+++) :: T.Text -> T.Text -> T.Text
(+++) = T.append

showForm :: Formula -> T.Text
showForm (Atom p) = p
showForm (Not (Atom p)) = "¬" +++ p
showForm (Not p) = "¬(" +++ showForm p +++ ")"
showForm o@(And p q) = precedence o p "∧" q
showForm o@(Or p q)  = precedence o p "∨" q
showForm o@(Xor p q) = precedence o p "⊕" q
showForm o@(If p q)  = precedence o p " ⇒ " q
showForm o@(Iff p q) = precedence o p " ⇔ " q

precedence :: Formula -> Formula -> T.Text -> Formula -> T.Text
precedence o p op q
  | p <= o && q <= o = showForm p +++ op +++ showForm q
  | q <= o = "(" +++ showForm p +++ ")" +++ op +++ showForm q
  | p <= o = showForm p +++ op +++ "(" +++ showForm q +++ ")"
  | otherwise = "(" +++ showForm p +++ ")" +++ op +++ "(" +++ showForm q +++ ")"
