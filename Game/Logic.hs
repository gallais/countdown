module Game.Logic where

import Data.List
import Data.Trie

data Error = NotInDict | IllegalChar

instance Show Error where
  show NotInDict   = "Your word is not in the dictionary!"
  show IllegalChar = "You are using illegal characters!"

isInDict :: Ord a => [a] -> Trie a -> Either Error ()
isInDict guess dict
  | isInTrie guess dict = return ()
  | otherwise           = Left NotInDict

isSubList :: Eq a => [a] -> [a] -> Either Error ()
isSubList []          pool       = return ()
isSubList guess       []         = Left IllegalChar
isSubList (g : guess) (p : pool)
  | g == p    = isSubList guess pool
  | otherwise = isSubList (g : guess) pool

isValid :: Ord a => [a] -> [a] -> Either Error ()
isValid guess pool = sort guess `isSubList` sort pool

score :: Ord a => [a] -> [a] -> Trie a -> Either Error Int
score guess pool dict = do
  valid   <- isValid  guess pool
  correct <- isInDict guess dict
  return $ if sc == 9 then 18 else sc
  where sc = length guess

isBest :: String -> String -> Trie Char -> String
isBest guess pool dict =
  let max = findMax pool dict
  in if length guess < length max
  then "A better solution would be: " ++ max
  else "And it is the best solution possible!"
