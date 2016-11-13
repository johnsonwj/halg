module Halg.Sort.InsertionSort (sort) where

-- inserts an element in the correct place in an already-sorted list
-- the inserted element is placed in front of any existing elements that compare equal
insertSorted :: (Ord a) => [a] -> a -> [a]
insertSorted xs i = let
  (as, bs) = break (>= i) xs
  in as ++ (i : bs)

sort :: (Ord a) => [a] -> [a]
sort = foldl insertSorted []
