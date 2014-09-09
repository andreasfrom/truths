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

prettyTable :: T.Text -> T.Text -> Form T.Text -> T.Text
prettyTable t f form = T.intercalate "\t" (atoms form) +++ "\t" +++ showForm form +++ "\n" +++ go (table form)
  where go [] = ""
        go ((as,b):rest) = format as +++ showBool t f b +++ "\n" +++ go rest
        format [] = ""
        format ((_,p):bs) = showBool t f p +++ "\t" +++ format bs

showBool :: T.Text -> T.Text -> Bool -> T.Text
showBool t _ True = t
showBool _ f False = f

-- Needs cleaning up
equal :: T.Text -> T.Text -> Formula -> Formula -> Either T.Text ()
equal tr fa f g | length f' /= length g' =
              Left "the propositions contain a different number of atoms"
          | not (null zipped) =
              Left ("the propositions differ in the case(s) below:\n"
                    +++ T.intercalate "\t" (zipWith (\p q -> if p==q then p else p +++ "|" +++ q) (atoms f) (atoms g))
                    +++ "\t" +++ showForm f +++ "\t" +++ showForm g
                    +++ "\n" +++ format (filter (not . snd) zipped))
          | otherwise = Right ()
  where zipper (as,b) (as',c) = (((as,b), (as',c)), b == c)
        f' = table f
        g' = table g
        zipped = filter (not . snd) $ zipWith zipper f' g'
        format [] = ""
        format ((((fs,fb), (_,gb)),_):rest) = T.intercalate "\t" (map (showBool tr fa . snd) fs) +++ "\t"
                                               +++ showBool tr fa fb +++ "\t" +++ showBool tr fa gb
                                               +++ "\n" +++ format rest

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
                              Single p -> (putStrLn $ T.unpack $ prettyTable t f p) >> loop (t, f)
                              Equality p q -> (case (equal t f p q) of
                                               Left e -> putStrLn (T.unpack e)
                                               Right () -> putStrLn (T.unpack t)) >> loop (t, f)
              Left e -> print e >> loop (t, f)

        fltr [x,y] = return (T.pack x, T.pack y)
        fltr _ = return ("T", "F")
