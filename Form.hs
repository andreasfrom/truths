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
            deriving (Functor, F.Foldable, Eq)

instance Show (Formula) where
  show = T.unpack . showForm

infixr 5 +++
(+++) :: T.Text -> T.Text -> T.Text
(+++) = T.append


-- This is ugly, but it works for now
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
precedence (If p q@(If _ _)) _ op _ = showForm p +++ op +++ "(" +++ showForm q +++ ")"
precedence (If p@(If _ _) q) _ op _ = "(" +++ showForm p +++ ")" +++ op +++ showForm q

precedence (Iff p@(If _ _) q@(If _ _)) _ op _ = "(" +++ showForm p +++ ")" +++ op +++ "(" +++ showForm q +++ ")"
precedence (Iff p q@(If _ _)) _ op _ = showForm p +++ op +++ "(" +++ showForm q +++ ")"
precedence (Iff p@(If _ _) q) _ op _ = "(" +++ showForm p +++ ")" +++ op +++ showForm q

precedence (If p@(Iff _ _) q@(Iff _ _)) _ op _ = "(" +++ showForm p +++ ")" +++ op +++ "(" +++ showForm q +++ ")"
precedence (If p q@(Iff _ _)) _ op _ = showForm p +++ op +++ "(" +++ showForm q +++ ")"
precedence (If p@(Iff _ _) q) _ op _ = "(" +++ showForm p +++ ")" +++ op +++ showForm q

precedence o p op q
  | p <= o && q <= o = showForm p +++ op +++ showForm q
  | q <= o = "(" +++ showForm p +++ ")" +++ op +++ showForm q
  | p <= o = showForm p +++ op +++ "(" +++ showForm q +++ ")"
  | otherwise = "(" +++ showForm p +++ ")" +++ op +++ "(" +++ showForm q +++ ")"

-- Ordering is based purely on precedence
instance Eq a => Ord (Form a) where
  Atom _ <= _ = True

  Not _ <= Atom _ = False
  Not _ <= _ = True

  And _ _ <= Atom _ = False
  And _ _ <= Not _ = False
  And _ _ <= _ = True

  Or _ _ <= Atom _ = False
  Or _ _ <= Not _ = False
  Or _ _ <= _ = True

  Xor _ _ <= Atom _ = False
  Xor _ _ <= Not _ = False
  Xor _ _ <= _ = True

  If _ _ <= Atom _ = False
  If _ _ <= Not _ = False
  If _ _ <= And _ _ = False
  If _ _ <= Or _ _ = False
  If _ _ <= Xor _ _ = False
  If _ _ <= _ = True

  Iff _ _ <= Atom _ = False
  Iff _ _ <= Not _ = False
  Iff _ _ <= And _ _ = False
  Iff _ _ <= Or _ _ = False
  Iff _ _ <= Xor _ _ = False
  Iff _ _ <= _ = True
