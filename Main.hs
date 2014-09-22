{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad           (forever, replicateM)
import qualified Data.Foldable           as F
import           Data.List               (nub)
import           Data.Maybe              (fromJust)
import qualified Data.Text               as T
import           Form
import           Parser
import           System.Console.Readline (addHistory, readline)
import           System.Environment      (getArgs)
import           System.Exit             (exitSuccess)
import           Table

assign :: Form T.Text -> Assignments -> Form Bool
assign form as = fmap (fromJust . flip lookup as) form

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

table :: Form T.Text -> [(Assignments, Bool)]
table form = zip rows results
  where rows = cases (atoms form)
        results = map truth (map (assign form) rows)

buildTable :: ShowBool -> Form T.Text -> TruthTable
buildTable showB form = TruthTable $ (atoms form ++ [showForm form]) : foldl go [] (table form)
  where go acc (as,b) = (foldl (\ps (_,p) -> showB p : ps) [] as ++ [showB b]) : acc

type ShowBool = Bool -> T.Text
showBool :: T.Text -> T.Text -> ShowBool
showBool t _ True = t
showBool _ f False = f

equivalent :: ShowBool -> Formula -> Formula -> Either T.Text ()
equivalent showB f g
  | length (table f) /= length (table g) = Left "the propositions contain a different number of atoms"
  | not (null different) =
      Left $ "the propositions differ in the cases below:\n"
      +++ showTable (TruthTable ([zipWith name (atoms f) (atoms g) ++ [showForm f] ++ [showForm g]]
                                 ++ map (format . fst) different))
  | otherwise = Right ()
  where different = filter (not . snd) $ zipWith differ (table f) (table g)
        differ (as,b) (_,c) = ((as,b,c), b == c)
        format (fs,fb,gb) = (map (showB . snd) fs) ++ (map showB [fb,gb])
        name a b | a == b = a
        name a b = a +++ "/" +++ b

main :: IO ()
main = putStrLn "Enter a proposition (or \"exit\"):" >> getArgs >>= fltr >>= \(t,f) -> forever $ do
  prop <- readline "> "
  case prop of
   Nothing -> return ()
   Just "exit" -> exitSuccess
   Just line ->
     addHistory line >>
     case (parseInput (T.pack line)) of
      Left e -> print e
      Right prop' -> case prop' of
        Single p -> (putStrLn $ show $ buildTable (showBool t f) p)
        Equivalence p q -> case equivalent (showBool t f) p q of
          Left e -> putStrLn $ T.unpack e
          Right () -> putStrLn $ T.unpack t

  where fltr [x,y] = return (T.pack x, T.pack y)
        fltr _ = return ("T", "F")
