module Data.Trie where

import Data.List

type Tries a = [(a, Trie a)]

data Trie a = Node [[a]] (Tries a)
  deriving (Eq, Show)

leaf :: Trie a
leaf = Node [] []

insertTrie :: Ord a => [a] -> Trie a -> Trie a
insertTrie word trie = go word (sort word) trie
  where
  go whole [] (Node ws ts) = Node (whole : ws) ts
  go whole as (Node ws ts) = Node ws (gos whole as ts)

  gos whole (a : as) [] = [(a , go whole as leaf)]
  gos whole (a : as) ((b, t) : ts)
    | a == b    = (b, go whole as t) : ts
    | otherwise = (b, t) : gos whole (a : as) ts

buildTrie :: Ord a => [[a]] -> Trie a
buildTrie = foldl (flip insertTrie) leaf

isInTrie :: Ord a => [a] -> Trie a -> Bool
isInTrie word trie = go word (sort word) trie
  where
  go whole [] (Node ws ts) = whole `elem` ws
  go whole as (Node ws ts) = gos whole as ts

  gos whole (a : as) [] = False
  gos whole (a : as) ((b, t) : ts)
    | a == b    = go whole as t
    | otherwise = gos whole (a : as) ts

maxLength :: [a] -> [a] -> [a]
maxLength xs ys =
  if length xs < length ys
  then ys
  else xs

findMax :: Ord a => [a] -> Trie a -> [a]
findMax w t = go [] (sort w) t
  where
    go acc as (Node [] ts)       = gos acc as ts
    go acc as (Node (w : ws) ts) = gos (maxLength w acc) as ts

    gos acc [] _  = acc
    gos acc _  [] = acc
    gos acc (a : as) ((b, t) : ts)
      | a == b    = k $ go acc as t
      | otherwise = k acc
      where
        k = \ acc -> gos (gos acc (a : as) ts) as ((b, t) : ts)
