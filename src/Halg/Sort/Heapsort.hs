module Halg.Sort.Heapsort (sort) where

import Data.Array

type BinaryHeap a = Array Int a

heapSize :: BinaryHeap a -> Int
heapSize h
  | lb <= ub  = ub - lb + 1
  | otherwise = 0
  where (lb, ub) = bounds h

exch :: Int -> Int -> BinaryHeap a -> BinaryHeap a
exch i j h = h // [(i, h!j), (j, h!i)]

-- bubbles the given element in the heap up until it satisfies heap order
bubble :: (Ord a) => Int -> BinaryHeap a -> BinaryHeap a
bubble i h
  | i == 1            = h
  | h!i < h!p         = bubble p (exch i p h)
  | otherwise         = h
  where
    p = i `quot` 2

-- sinks by comparing parent with both children (assumed to exist)
sink2 :: (Ord a) => (Int, Int, Int) -> BinaryHeap a -> BinaryHeap a
sink2 (p, lc, rc) h
  | h!p < h!lc && h!p < h!rc  = h
  | h!lc < h!rc               = exch p lc h
  | otherwise                 = exch p rc h

-- sinks by comparing parent with single child (assumed to exist, and to be only child)
sink1 :: (Ord a) => (Int, Int) -> BinaryHeap a -> BinaryHeap a
sink1 (p, c) h
  | h!p < h!c = h
  | otherwise = exch p c h

-- sinks the given element in the heap down until it satisfies heap order
-- delegates comparisons to sink1 or sink2 depending on leaf/1-node/2-node status
sink :: (Ord a) => Int -> BinaryHeap a -> BinaryHeap a
sink i h
  | rc <= heapSize h = sink2 (i, lc, rc) h
  | lc <= heapSize h = sink1 (i, lc) h
  | otherwise        = h
  where
    lc = 2*i
    rc = 2*i + 1

heapInsert :: (Ord a) => BinaryHeap a -> a -> BinaryHeap a
heapInsert h x =
  let
    newSize = heapSize h + 1
  in bubble newSize $ array (1, newSize) $ (newSize, x) : assocs h

heapify :: (Ord a) => [a] -> BinaryHeap a
heapify = foldl heapInsert (array (1, 0) [])

removeMin :: (Ord a) => BinaryHeap a -> (a, BinaryHeap a)
removeMin h = (h!1, rest)
  where
    rest =
      let
        oldSize = heapSize h
        newSize = oldSize - 1
      in sink 1 $ array (1, newSize) $ (1, h!oldSize) : filter (\(i, _) -> i < oldSize && i > 1) (assocs h)

sort' :: (Ord a) => BinaryHeap a -> [a]
sort' h
  | heapSize h == 0 = []
  | heapSize h == 1 = [h!1]
  | otherwise       = let (minX, rest) = removeMin h in minX : sort' rest

sort :: (Ord a) => [a] -> [a]
sort = sort' . heapify
