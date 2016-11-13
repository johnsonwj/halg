module Halg.Sort.Quicksort where

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = let
  begin = sort $ filter (<= x) xs
  end   = sort $ filter  (> x) xs
  in begin ++ (x : end)