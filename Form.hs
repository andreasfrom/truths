{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Form where

import           Control.Monad (replicateM)
import qualified Data.Foldable as F
import           Data.List     (nub)
import           Data.Monoid   ((<>))
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

atoms :: Form T.Text -> [T.Text]
atoms = nub . F.foldr (++) [] . fmap (\x -> [x])

cases :: [T.Text] -> [Assignments]
cases names = map (zip names) (replicateM (length names) [True, False])

truth :: Form Bool -> Bool
truth (Atom p) = p
truth (Not p) = not (truth p)
truth (And p q) = truth p && truth q
truth (Or p q) = truth p || truth q
truth (Xor p q) = truth p /= truth q
truth (If p q) = not (truth p) || truth q
truth (Iff p q) = truth p == truth q

instance Show (Formula) where
  show = T.unpack . showForm

type ShowConnective = Formula -> T.Text
symbolConnective :: ShowConnective
symbolConnective (Not _) = "¬"
symbolConnective (And _ _) = "∧"
symbolConnective (Or _ _)  = "∨"
symbolConnective (Xor _ _) = "⊕"
symbolConnective (If _ _)  = " ⇒ "
symbolConnective (Iff _ _) = " ⇔ "

showForm :: Formula -> T.Text
showForm = showCustomForm symbolConnective

showCustomForm :: ShowConnective -> Formula -> T.Text
showCustomForm _ (Atom p) = p
showCustomForm showC o@(Not (Atom p)) = showC o <> p
showCustomForm showC (Not p) = "¬(" <> showCustomForm showC p <> ")"
showCustomForm showC o@(And p q) = parens (showCustomForm showC) (grade o) (grade p) (showC o) (grade q)
showCustomForm showC o@(Or p q)  = parens (showCustomForm showC) (grade o) (grade p) (showC o) (grade q)
showCustomForm showC o@(Xor p q) = parens (showCustomForm showC) (grade o) (grade p) (showC o) (grade q)
showCustomForm showC o@(If p q)  = parens (showCustomForm showC) (grade o) (grade p) (showC o) (grade q)
showCustomForm showC o@(Iff p q) = parens (showCustomForm showC) (grade o) (grade p) (showC o) (grade q)

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

parens :: (Formula -> T.Text) -> Precedence T.Text -> Precedence T.Text -> T.Text -> Precedence T.Text -> T.Text
parens showF (Precedence _ ol ot oa) (Precedence p pl pt _) op (Precedence q ql qt _)
  | ot == pt && oa == Associative = (if ql < ol then plains else rights) showF p op q
  | ot == qt && oa == Associative = (if pl < ol then plains else lefts) showF p op q
  | ot == pt && ql < ol && oa == AssocLeft = lefts showF p op q
  | ot == qt && pl < ol && oa == AssocRight = rights showF p op q
  | pl < ol && ql < ol = plains showF p op q
  | pl < ol = rights showF p op q
  | ql < ol = lefts showF p op q
  | otherwise = boths showF p op q

plains, lefts, rights, boths :: (Formula -> T.Text) -> Formula -> T.Text -> Formula -> T.Text
plains showF p op q = showF p <> op <> showF q
lefts showF p op q = "(" <> showF p <> ")" <> op <> showF q
rights showF p op q = showF p <> op <> "(" <> showF q <> ")"
boths showF p op q = "(" <> showF p <> ")" <> op <> "(" <> showF q <> ")"
