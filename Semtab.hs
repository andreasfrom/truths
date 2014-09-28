{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Semtab where

import           Data.Monoid          ((<>))
import qualified Data.Text            as T
import           Diagrams.Backend.SVG
import           Diagrams.Prelude     hiding ((<>))
import           Diagrams.TwoD.Arrow
import           Form
import           Graphics.SVGFonts
import           Table                (showBool)

text' :: String -> Diagram B R2
text' t = stroke (textSVG' $ TextOpts t lin INSIDE_H KERN False 1 10)
           # fc black # fillRule EvenOdd

tabToDia :: FormTab -> Diagram B R2
tabToDia (Rule top@(Case (Just 1) _ _) p t c n) = tabToDia (Cons top (Rule (Case Nothing undefined undefined) p t c n))

tabToDia Closed = text' "X"
tabToDia (Cons a Closed) = tabToDia a === strutY 1 === vrule 6 === strutY 1 === text' "X"
tabToDia Open = text' "O"
tabToDia (Cons a Open) = tabToDia a === vrule 8 === text' "O"

tabToDia (Case n p t) = text' ((maybe "" ((++". ") . show) n) ++ (show p) ++ " : " ++ T.unpack (showBool "T" "F" t))

tabToDia (Cons a b) = tabToDia a === strutY 6 === tabToDia b

tabToDia (Rule _ p@(Branch _ _) c t n) = (cat' (r2 (1,1)) (with & sep .~ 8) [tabToDia p, (text' ((show c) ++ " " ++ T.unpack (showBool "T" "F" t) ++ " " ++ show n))])
                                         <> (arrowV' (with & tailGap .~ (Local 5) & headGap .~ (Local (-4)) & arrowHead .~ noHead)
                                         (r2 (-15,25)) # translateX (20))
                                         <> (arrowV' (with & tailGap .~ (Local 5) & headGap .~ (Local (-4)) & arrowHead .~ noHead)
                                         (r2 (15,25)) # translateX (-20))
tabToDia (Rule _ p c t n) = (cat' (r2 (1,1)) (with & sep .~ 8) [tabToDia p, (text' ((show c) ++ " " ++ T.unpack (showBool "T" "F" t) ++ " " ++ show n))])
                            <> arrowV' (with & tailGap .~ (Local 5) & headGap .~ (Local (-4)) & arrowHead .~ noHead) (r2 (0,25))

tabToDia (Branch a b) = cat' (r2 (1,0)) (with & sep .~ 25) [tabToDia a, tabToDia b] # translateX (-20)

-- Reconsider this
data Connective = CNot | CAnd | COr | CXor | CIf | CIff
                deriving (Eq)

-- This is stupid
instance Show Connective where
  show CNot = "¬"
  show CAnd = "∧"
  show COr = "∨"
  show CXor = "⊕"
  show CIf = "⇒ "
  show CIff = "⇔ "

data Tab a = Closed | Open
           | Case (Maybe Int) a Bool
           | Cons (Tab a) (Tab a)
           | Rule (Tab a) (Tab a) Connective Bool Int
           | Branch (Tab a) (Tab a)
           deriving (Show, Eq)

type FormTab = Tab Formula

showTab :: FormTab -> T.Text
showTab (Case (Just n) p t) = T.pack (show n) <> ". " <> showForm p <> " : " <> showBool "T" "F" t
showTab (Case Nothing p t) = showForm p <> " : " <> showBool "T" "F" t
showTab (Cons a b) = showTab a <> "\n" <> showTab b
showTab (Rule top@(Case (Just 1) _ _) p c t n) = showTab top <> "\n"
  <> "  | " <> T.pack (show c) <> showBool "T" "F" t <> " " <> T.pack (show n) <> "\n" <> showTab p
showTab (Rule _ p c t n) = "  | " <> T.pack (show c) <> showBool "T" "F" t <> " " <> T.pack (show n) <> "\n" <> showTab p
showTab (Branch a b) = "[" <> showTab a <> "] <>\n<> [" <> showTab b <> "]"
showTab Closed = "  |\n  X"
showTab Open = "  |\n  O"

example :: FormTab
example = Case (Just 1) (If (And (Or (Atom "p") (Atom "q")) (Not (Atom "q"))) (Atom "p")) False

reduceTab :: FormTab -> FormTab
reduceTab (Case _ a@(Atom _) t) = (Case Nothing a t)
reduceTab c@(Case (Just n) prop bool) =
   Rule c res (toConnective prop) bool n
  where sub = go prop bool
        res = if isReducible sub then (Cons sub (reduceTab sub)) else sub
        go (Not p) t = Case (if isAtom p then Nothing else Just (n+1)) p (not t)
        go (And p q) True = cons p True q True
        go (And p q) False = branch p False q False
        go (Or p q) True = branch p True q True
        go (Or p q) False = cons p False q False
        go (Xor p q) True = Branch (cons p True q False) (cons p False q True)
        go (Xor p q) False = Branch (cons p True q True) (cons p False q False)
        go (If p q) True = branch p False q True
        go (If p q) False = cons p True q False
        go (Iff p q) True = Branch (cons p True q True) (cons p False q False)
        go (Iff p q) False = Branch (cons p True q False) (cons p False q True)

        cons = bicase Cons
        branch = bicase Branch
        bicase ctor p s q t = ctor (Case (maybeNum p q 1) p s) (Case (maybeNum p q 2) q t)

        maybeNum :: Form a -> Form a -> Int -> Maybe Int
        maybeNum (Atom _) _ 1 = Nothing
        maybeNum _  _ 1 = Just (n+1)
        maybeNum _  (Atom _) 2 = Nothing
        maybeNum _ _ 2 = Just (n+2)

reduceTab c@(Cons a b)
  | isReducible a && isReducible b = Cons (reduceTab a) (reduceTab b)
  | isReducible a = (reduceTab a)
  | isReducible b = (reduceTab b)
  | otherwise = c

reduceTab c@(Rule _ b _ _ _)
  | isReducible b = Cons c (reduceTab b)
  | otherwise = c

reduceTab c@(Branch a b)
  | isReducible a && isReducible b = Branch (reduceTab a) (reduceTab b)
  | isReducible a = Cons b (reduceTab a)
  | isReducible b = Cons a (reduceTab b)
  | otherwise = c

type Env = [(Formula, Bool)]

closes :: Env -> FormTab -> Bool
closes env (Case _ p t) = case lookup p env of
  Nothing -> False
  Just t' -> t /= t'
closes _ _ = False

rx :: FormTab
rx = (Cons (Cons (Case Nothing (Atom "p") True) Closed) (Rule (Case (Just 4) (Not (Atom "q")) True) (Case Nothing (Atom "q") False) CNot True 4))

closeBranches :: (FormTab, Env) -> (FormTab, Env)
closeBranches (c@(Case _ p t), env) = if closes env c then (Cons c Closed, env) else (c, (p,t):env)
closeBranches (Cons a b, env) = (Cons first second, env'')
  where (first, env') = closeBranches (a, env)
        (second, env'') = closeBranches (b, env')

closeBranches (Rule a p x y z, env) = (Rule a closed x y z, env')
  where (closed, env') = closeBranches (p, env)
closeBranches (Branch a b, env) = (Branch first second, env'++env'')
  where (first, env') = closeBranches (a, env)
        (second, env'') = closeBranches (b, env)

rewrite :: FormTab -> FormTab
rewrite Closed = Closed
rewrite Open = Open
rewrite c@(Case _ _ _) = c
rewrite (Cons (Branch x y) a) = Branch (Cons x (rewrite a)) (Cons y (rewrite a))
rewrite (Cons (Rule o (Branch x y) p q r) a) = Rule o (Branch (Cons x a) (Cons y a)) p q r
rewrite (Cons Closed _) = Closed
rewrite (Cons c@(Cons _ Closed) _) = c
rewrite (Cons Open _) = Open
rewrite (Cons c@(Cons _ Open) _) = c
rewrite (Cons a b) = Cons (rewrite a) (rewrite b)
rewrite (Branch a b) = Branch (rewrite a) (rewrite b)
rewrite (Rule o a p q r) = Rule o (rewrite a) p q r

isAtom :: Form a -> Bool
isAtom (Atom _) = True
isAtom _ = False

isReducible :: FormTab -> Bool
isReducible (Case _ (Atom _) _) = False
isReducible (Cons a b) = isReducible a || isReducible b
isReducible (Rule _ b _ _ _) = isReducible b
isReducible (Branch a b) = isReducible a || isReducible b
isReducible _ = True

toConnective :: Form a -> Connective
toConnective (Not _) = CNot
toConnective (And _ _) = CAnd
toConnective (Or _ _) = COr
toConnective (Xor _ _) = CXor
toConnective (If _ _) = CIf
toConnective (Iff _ _) = CIff
