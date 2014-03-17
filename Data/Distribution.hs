module Data.Distribution where

import System.Random

type Distr a = [(Int, a)]

incFreq :: Eq a => a -> Distr a -> Distr a
incFreq a [] = [(1, a)]
incFreq a ((k, b) : ds)
  | a == b    = (k+1, b) : ds
  | otherwise = (k, b) : incFreq a ds

incFreqs :: Eq a => [a] -> Distr a -> Distr a
incFreqs as ds = foldl (flip incFreq) ds as

getValue :: Int -> Distr a -> a
getValue m [] = undefined
getValue m ((n, d) : ds)
  | m < n     = d
  | otherwise = getValue (m - n) ds

rand :: Distr a -> IO a
rand ds = do
  sum <- return . sum . map fst $ ds
  idx <- getStdRandom (randomR (0,sum-1))
  return $ getValue idx ds
