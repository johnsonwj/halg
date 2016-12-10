module Halg.Sort.SelectionSort (sort) where

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort xs = let
  (as, b:bs) = break (\x -> x == minimum xs) xs
  in b : sort (as ++ bs)
