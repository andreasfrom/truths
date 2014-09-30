{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Semtab where

import           Data.Monoid          ((<>))
import qualified Data.Text            as T
import           Diagrams.Backend.SVG
import           Diagrams.Prelude     hiding (trace, (<>))
import           Form
import           Graphics.SVGFonts
import           Table                (showBool)

-- A Graphviz/Dot backend would be cool

text' :: T.Text -> Diagram B R2
text' t = stroke (textSVG' $ TextOpts (T.unpack t) lin INSIDE_H KERN False 1 1) # fillRule EvenOdd # fc black

tabToDia :: FormTab -> Double -> Diagram B R2
tabToDia Closed _ = vrule 1 === strutY 0.2 === text' "X"
tabToDia Open _ = vrule 1 === strutY 0.2 === text' "O"

tabToDia (Case n p t) _ = text' ((maybe "" ((<>". ") . T.pack . show) n) <> showForm p <> " : " <> showBool "T" "F" t)

tabToDia (Cons a b) indent = cat' (r2 (0,-1)) (with & sep .~ 0.5) [tabToDia a indent, tabToDia b indent]

tabToDia (Rule q p@(Branch _ _) t n) indent =
  (if n == 1 then tabToDia (Case (Just 1) q t) indent === strutY 0.5 else mempty)
  ===
  (((if indent < 3 then (cat' (r2 (2,1)) (with & catMethod .~ Cat))
     else (cat' (r2 (0,1)) (with & catMethod .~ Distrib & sep .~ indent/4))) [tabToDia p indent, diaLabel q t n])
   <> (arrowV' (with & tailGap .~ (Local 1) & arrowHead .~ noHead)
       (r2 (-(indent*0.4),indent/2)) # translateX (indent/2))
   <> (arrowV' (with & tailGap .~ (Local 1) & arrowHead .~ noHead)
       (r2 (indent*0.4,indent/2))) # translateX (-(indent/2)))

tabToDia (Rule q p t n) indent =
  (if n == 1 then tabToDia (Case (Just 1) q t) indent else mempty)
  === ((cat' (r2 (1,1)) (with & sep .~ 2 & catMethod .~ Distrib) [tabToDia p indent, diaLabel q t n])
       <> arrowV' (with & tailGap .~ (Local 0.5) & arrowHead .~ noHead) (r2 (0,2)))

tabToDia (Branch a b) indent = cat' (r2 (1,0)) (with & sep .~ indent & catMethod .~ Distrib)
                               [tabToDia a (indent*0.5), tabToDia b (indent*0.5)] # translateX (-(indent/2))

diaLabel :: Formula -> Bool -> Int -> Diagram B R2
diaLabel q t n = text' (symbolConnective q <> " " <> showBool "T" "F" t <> " " <> T.pack (show n))

renderTab :: FilePath -> FormTab -> IO ()
renderTab fp p = renderSVG fp (Height 600) (tabToDia (process p) 10)

process :: FormTab -> FormTab
process = openBranches . closeBranches . giveNumbers . reduceTab

data Tab a = Closed | Open
           | Case (Maybe Int) a Bool
           | Cons (Tab a) (Tab a)
           | Rule a (Tab a) Bool Int
           | Branch (Tab a) (Tab a)
           deriving (Show, Eq)

type FormTab = Tab Formula

example1, example2, example3, example4, example5, example6 :: FormTab
example1 = Case Nothing (If (And (Or (Atom "p") (Atom "q")) (Not (Atom "q"))) (Atom "p")) False
example2 = Case Nothing (Or (Atom "p") (Not (Atom "p"))) True
example3 = Case Nothing (If (And (Or (Atom "p") (Atom "q")) (Not (Atom "q"))) (Atom "p")) True
example4 = Case Nothing (If (Iff (And (Atom "p") (Atom "q")) (Atom "p")) (Iff (Atom "p") (Atom "q"))) False
example5 = Case Nothing (If (Iff (And (Atom "p") (Atom "q")) (Atom "p")) (Iff (Atom "p") (Atom "q"))) True
example6 = Case Nothing (Iff (Xor (Xor (Atom "p") (Atom "q")) (Atom "p")) (Atom "q")) False

reduceTab :: FormTab -> FormTab
reduceTab (Case _ a@(Atom _) t) = (Case Nothing a t)
reduceTab (Case _ prop bool) = reduceTab $ Rule prop sub bool undefined
  where sub = go prop bool
        go (Not p) t = Case Nothing p (not t)
        go (And p q) True = cons' p True q True
        go (And p q) False = branch p False q False
        go (Or p q) True = branch p True q True
        go (Or p q) False = cons' p False q False
        go (Xor p q) True = Branch (cons' p True q False) (cons' p False q True)
        go (Xor p q) False = Branch (cons' p True q True) (cons' p False q False)
        go (If p q) True = branch p False q True
        go (If p q) False = cons' p True q False
        go (Iff p q) True = Branch (cons' p True q True) (cons' p False q False)
        go (Iff p q) False = Branch (cons' p True q False) (cons' p False q True)

        cons' = bicase cons
        branch = bicase Branch
        -- Number everything Nothing right now, I'll traverse the tree afterwards numbering it breadth-first
        bicase ctor p s q t = ctor (Case Nothing p s) (Case Nothing q t)

reduceTab c@(Cons a b)
  | isReducible a && isReducible b = if rank a <= rank b
                                     then cons (reduceTab a) (reduceTab b)
                                     else cons (reduceTab b) (reduceTab a)
  | isReducible a = reduceTab a
  | isReducible b = reduceTab b
  | otherwise = c

reduceTab r@(Rule o b c n)
  | isReducible b = Rule o (cons b (reduceTab b)) c n
  | otherwise = r

reduceTab c@(Branch a b)
  | isReducible a && isReducible b = Branch (reduceTab a) (reduceTab b)
  | isReducible a = reduceTab a
  | isReducible b = reduceTab b
  | otherwise = c

rank :: FormTab -> Int
rank (Case _ a t) = go a t
  where go (Not _) _ = 0
        go (And _ _) True = 1
        go (Or _ _) False = 1
        go (If _ _) False = 1
        go (And _ _) False = 2
        go (Or _ _) True = 2
        go (If _ _) True = 2
        go _ _ = 3
rank _ = 4


giveNumbers :: FormTab -> FormTab
giveNumbers tab = fst $ go tab 2 []
  where go :: FormTab -> Int -> [(Formula, Int)] -> (FormTab, (Int, [(Formula, Int)]))
        go c@(Case _ (Atom _) _) n env = (c, (n, env))
        go (Case _ p t) n env = (Case (Just n) p t, (n+1, (p,n):env))
        go (Cons a b) n env = (cons a' b', (n'',env''))
          where (a',(n',env')) = go a n env
                (b',(n'',env'')) = go b n' env'
        go (Branch a b) n env = (Branch a' b', (n'', env''))
          where (a',(n',env')) = go a n env
                (b',(n'',env'')) = go b n' env'
        go (Rule o p t _) n env = (Rule o p' t m', (n',env')) -- m here needs to be updated
          where (p',(n',env')) = go p n env
                m' = maybe 1 id (lookup o env)
        go x n e = (x, (n, e))

type Env = [(Formula, Bool)]

closes :: Env -> (Formula, Bool) -> Bool
closes env (p,t) = case lookup p env of
  Just t' -> t /= t'
  Nothing -> False

closeBranches :: FormTab -> FormTab
closeBranches tab = go tab []
  where go :: FormTab ->  Env -> FormTab
        go c@(Case _  p t) env = if closes env (p,t) then Cons c Closed else c
        go (Cons a@(Case Nothing p t) x) env =
          if closes env (p,t) then Cons a (cons x Closed) else Cons a (go x ((p,t):env))
        go (Cons a b) env = cons a (go b env)
        go (Rule o p c n) env = Rule o (go p env) c n
        go (Branch a b) env = Branch (go a env) (go b env)

-- This is testimony to the bad design of my data type
cons :: FormTab -> FormTab -> FormTab
cons (Branch x y) (Branch a b) = Branch (if isReducible x then (cons x a) else x) (if isReducible y then (cons y b) else y)
cons (Branch x y) a = Branch (if isReducible x then (cons x a) else x) (if isReducible y then (cons y a) else y)
cons (Rule o (Branch x y) c n) a = Rule o (Branch (cons x a) (cons y a)) c n
cons (Rule o p c n) a = Rule o (cons p a) c n
cons (Cons x y) a = cons x (cons y a)
cons x a = Cons x a

pruneClosed :: FormTab -> FormTab
pruneClosed c@(Case _ _ _) = c
pruneClosed (Cons Closed c) = Cons (pruneClosed c) Closed
pruneClosed c@(Cons _ Closed) = c
pruneClosed (Cons a b) = Cons (pruneClosed a) (pruneClosed b)
pruneClosed (Branch x y) = Branch (pruneClosed x) (pruneClosed y)
pruneClosed (Rule o p c n) = Rule o (pruneClosed p) c n

openBranches :: FormTab -> FormTab
openBranches (Cons a b@(Case _ _ _)) = Cons a (Cons b Open)
openBranches (Cons a b) = Cons a (openBranches b)
openBranches (Branch x@(Case Nothing _ _) y@(Case _ _ _)) = Branch (Cons x Open) (Cons y Open)
openBranches (Branch x@(Case Nothing _ _) y) = Branch (Cons x Open) (openBranches y)
openBranches (Branch x y@(Case Nothing _ _)) = Branch (openBranches x) (Cons y Open)
openBranches (Branch x y) = Branch (openBranches x) (openBranches y)
openBranches (Rule o p@(Case Nothing _ _) c n) = Rule o (Cons p Open) c n
openBranches (Rule o p c n) = Rule o (openBranches p) c n
openBranches x = x

isAtom :: Form a -> Bool
isAtom (Atom _) = True
isAtom _ = False

isReducible :: FormTab -> Bool
isReducible (Case _ (Atom _) _) = False
isReducible (Case _ _ _) = True
isReducible (Cons a b) = isReducible a || isReducible b
isReducible (Rule _ b _ _) = isReducible b
isReducible (Branch a b) = isReducible a || isReducible b
