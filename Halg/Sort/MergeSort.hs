module Halg.MergeSort (sort) where

-- merges two sorted lists into another sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort x = let
  iHalf = quot (length x) 2
  firstHalf = take iHalf x
  secondHalf = drop iHalf x 
  in merge (sort firstHalf) (sort secondHalf)
