module Main where

import System.IO
import System.Process

import Control.Monad
import Data.List

import Data.Trie
import Data.Distribution

import Game.Logic

splitVowelConsonant :: Distr Char -> (Distr Char, Distr Char)
splitVowelConsonant = partition ((`elem` "aeiouy") . snd)

buildDict :: String -> IO (Trie Char, (Distr Char, Distr Char))
buildDict file = do
  words <- fmap lines $ readFile file
  freqs <- return $ foldl (flip incFreqs) [] words
  return (buildTrie words, splitVowelConsonant freqs)

clear = system "clear"

squareBrackets :: String -> String
squareBrackets str = "[" ++ str ++ "]"

getExtraLetter :: Distr Char -> Distr Char -> String -> IO String
getExtraLetter v c str = go False
  where
    getLetter :: Char -> IO (Maybe Char)
    getLetter 'v' = rand v >>= return . Just
    getLetter 'c' = rand c >>= return . Just
    getLetter _   = return Nothing

    go flag = do
      _ <- clear
      putStrLn $
        (if flag then "Invalid character; " else "")
        ++ "Pick a character (v / c) " ++ squareBrackets str
      ch <- getChar
      getLetter ch >>= maybe (go True) (return . (: str))

buildPool :: Int -> Distr Char -> Distr Char -> IO String
buildPool n v c =
  foldM (flip . const $ getExtraLetter v c) "" [1..n]

getGuess :: String -> IO String
getGuess str = do
  _ <- clear
  putStrLn $ "Enter a word composed of the letters "
             ++ squareBrackets str
  getLine

game :: Trie Char -> Distr Char -> Distr Char -> IO ()
game dict vowel consonant = do
  pool  <- buildPool 9 vowel consonant
  guess <- getGuess pool
  print $ score guess pool dict
  putStrLn $ isBest guess pool dict
  menu False dict vowel consonant

waitFor :: (Char -> Maybe (IO a)) -> IO a
waitFor handle = getChar >>= maybe (waitFor handle) id . handle

menu :: Bool -> Trie Char -> Distr Char -> Distr Char -> IO ()
menu display d v c = do
  (if display
   then putStrLn " s - Start a new game \n q - Quit"
   else return ())
  waitFor handleMenu
  where
    handleMenu 's' = Just $ game d v c
    handleMenu 'q' = Just $ clear >> putStrLn "Have a nice day!"
    handleMenu _   = Just $ clear >> menu True d v c

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  (dict, (vowel, consonant))  <- buildDict "dict"
  menu True dict vowel consonant

