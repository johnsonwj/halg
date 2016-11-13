module Halg.Sort.ShellSort where

import Data.List (transpose)
import qualified Halg.Sort.InsertionSort

iSort :: (Ord a) => [a] -> [a]
iSort = Halg.Sort.InsertionSort.sort

-- splits a list up into sublists of the given length. (the last sublist may be smaller)
nSplit :: Int -> [a] -> [[a]]
nSplit n xs
  | length xs < n = [xs]
  | otherwise     = take n xs : nSplit n (drop n xs)

-- pops a value `h' from hs, and sorts each subsequence of every h values in xs.
hSort :: (Ord a) => [Int] -> [a] -> [a]
hSort hs xs
  | not (null hs) && head hs > 1  = let
                                      h:hs' = hs
                                      in hSort hs' $ concat . transpose . map iSort . transpose $ nSplit h xs
  | otherwise                     = iSort xs

hSeq :: Int -> [Int]
hSeq n = takeWhile (\x -> x < n `quot` 3) (iterate (\x -> 3 * x + 1) 1)

sort :: (Ord a) => [a] -> [a]
sort xs = hSort (hSeq $ length xs) xs
