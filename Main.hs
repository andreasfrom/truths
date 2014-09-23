{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad           (forever)
import qualified Data.Text               as T
import           Form
import           Parser
import           System.Console.Readline (addHistory, readline)
import           System.Environment      (getArgs)
import           System.Exit             (exitSuccess)
import           Table

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
        Single p -> putStrLn $ show $ defaultTable (showBool t f) p
        Context p -> putStrLn $ T.unpack $ contextTable p
        Latex p -> putStrLn $ T.unpack $ latexTable p
        Equivalence p q -> case equivalent (showBool t f) p q of
          Left e -> putStrLn $ T.unpack e
          Right () -> putStrLn $ T.unpack t

  where fltr [x,y] = return (T.pack x, T.pack y)
        fltr _ = return ("T", "F")
