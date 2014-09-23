{-# LANGUAGE OverloadedStrings #-}

module Table where

import           Data.Maybe (fromJust)
import qualified Data.Text  as T
import           Form

newtype TruthTable = TruthTable [[T.Text]]

instance Show TruthTable where
  show = T.unpack . showTable

showTable :: TruthTable -> T.Text
showTable (TruthTable (header:rows)) = T.intercalate "\n" (T.intercalate "\t" header : (map go rows))
  where go row = T.concat (zipWith (\col t -> col +++ T.concat (replicate t "\t")) row tabs)
        tabs = map (succ . (`div` 8) . T.length) header

type ShowBool = Bool -> T.Text
showBool :: T.Text -> T.Text -> ShowBool
showBool t _ True = t
showBool _ f False = f

table :: Form T.Text -> [(Assignments, Bool)]
table form = zip rows results
  where rows = cases (atoms form)
        results = map truth (map (assign form) rows)
        assign form' as = fmap (fromJust . flip lookup as) form'

defaultTable :: ShowBool -> Formula -> TruthTable
defaultTable = flip buildTable symbolConnective

buildTable :: ShowBool -> ShowConnective -> Form T.Text -> TruthTable
buildTable showB showC form = TruthTable $ (atoms form ++ [showCustomForm showC form]) : foldl go [] (table form)
  where go acc (as,b) = (foldl (\ps (_,p) -> showB p : ps) [] as ++ [showB b]) : acc

contextTable :: Formula -> T.Text
contextTable form =
  "\\starttabulate[|" +++ T.concat (replicate (length header) "c|") +++ "]\n"
  +++ go header +++ " \\HL\n" +++ T.intercalate "\n" (map go rows)
  +++ "\n\\stoptabulate"
  where go [] = ""
        go row = "  \\NC " +++ T.concat (map f (zip [0..] row)) +++ "\\NR"
        f (n, col) = col +++ (if n == length header - 2 then " \\VL " else " \\NC ")
        TruthTable (header:rows) = buildTable contextBool latexConnective form

latexTable :: Formula -> T.Text
latexTable form =
  "\\begin{tabular}{" +++ T.init (T.concat (replicate (length header) "c|")) +++ "}\n"
  +++ (T.init (T.init (go (init header)))) +++ " & $ " +++ (last header) +++ " $"  +++ " \\\\\\hline\n" +++ T.intercalate "\n" (map go rows)
  +++ "\n\\end{tabular}"
  where go [] = ""
        go row = "  " +++ T.concat (map (+++" & ") (init row)) +++ last row +++ "\\\\"
        TruthTable (header:rows) = buildTable latexBool latexConnective form
        latexBool = showBool "\\textsf{T}" "\\textsf{F}"


contextBool :: ShowBool
contextBool = showBool "{\\ss T}" "{\\ss F}"

latexConnective :: ShowConnective
latexConnective (Not _) = "\\lnot "
latexConnective (And _ _) = "\\land "
latexConnective (Or _ _)  = "\\lor "
latexConnective (Xor _ _) = "\\oplus "
latexConnective (If _ _)  = "\\rightarrow "
latexConnective (Iff _ _) = "\\leftrightarrow "
