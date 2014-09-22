{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad           (replicateM)
import qualified Data.Foldable           as F
import           Data.List               (nub)
import           Data.Maybe              (fromJust)
import qualified Data.Text               as T
import           Form
import           Parser
import           System.Console.Readline
import           System.Environment      (getArgs)

newtype TruthTable = TruthTable [[T.Text]]

instance Show TruthTable where
  show = T.unpack . showTable

showTable :: TruthTable -> T.Text
showTable (TruthTable (header:rows)) = T.intercalate "\n" (T.intercalate "\t" header : (map go rows))
  where go :: [T.Text] -> T.Text
        go row = T.concat (zipWith (\col t -> col +++ T.concat (replicate t "\t")) row tabs)
        tabs = map (succ . (`div` 8) . T.length) header

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

buildTable :: T.Text -> T.Text -> Form T.Text -> TruthTable
buildTable t f form = TruthTable $ (atoms form ++ [showForm form]) : foldl go [] (table form)
  where go acc (as,b) = (foldl (\ps (_,p) -> showBool t f p : ps) [] as ++ [showBool t f b]) : acc

showBool :: T.Text -> T.Text -> Bool -> T.Text
showBool t _ True = t
showBool _ f False = f

equivalent :: T.Text -> T.Text -> Formula -> Formula -> Either T.Text ()
equivalent tr fa f g
  | length (table f) /= length (table g) = Left "the propositions contain a different number of atoms"
  | not (null different) =
      Left $ "the propositions differ in the cases below:\n"
      +++ showTable (TruthTable ([zipWith name (atoms f) (atoms g) ++ [showForm f] ++ [showForm g]]
                                 ++ map (format . fst) different))
  | otherwise = Right ()
  where different = filter (not . snd) $ zipWith differ (table f) (table g)
        differ (as,b) (_,c) = ((as,b,c), b == c)
        format (fs,fb,gb) = (map (showBool tr fa . snd) fs) ++ (map (showBool tr fa) [fb,gb])
        name a b | a == b = a
        name a b = a +++ "/" +++ b

-- Needs cleaning up, could use the Reader monad for T F/1 0-configuration
main :: IO ()
main = putStrLn "Enter a proposition (or \"exit\"):" >> getArgs >>= fltr >>= loop
  where loop (t, f) = do
          prop <- readline "> "
          case prop of
           Nothing -> return ()
           Just "exit" -> return ()
           Just line ->
             addHistory line >>
             case (parseInput (T.pack line)) of
              Right prop' -> case prop' of
                              Single p -> (putStrLn $ show $ buildTable t f p) >> loop (t, f)
                              Equivalence p q -> (case equivalent t f p q of
                                                   Left e -> putStrLn $ T.unpack e
                                                   Right () -> putStrLn $ T.unpack t) >> loop (t, f)
              Left e -> print e >> loop (t, f)

        fltr [x,y] = return (T.pack x, T.pack y)
        fltr _ = return ("T", "F")
