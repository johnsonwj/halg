module Halg.MergeSort (mergeSort) where

-- merges two sorted lists into another sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort x = let
  iHalf = quot (length x) 2
  firstHalf = take iHalf x
  secondHalf = drop iHalf x 
  in merge (mergeSort firstHalf) (mergeSort secondHalf)
