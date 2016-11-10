module Halg.TwoThreeBST (TwoThreeTree, searchTree) where

import Control.Applicative

type NodeCompare = Ordering -- does this item belong before, inside, or after a node?

data TwoThreeTree a =
  EmptyTree
  | TwoNode a (TwoThreeTree a) (TwoThreeTree a)
  | ThreeNode a a (TwoThreeTree a) (TwoThreeTree a) (TwoThreeTree a)
  deriving (Show, Read, Eq)

searchSubtree :: (Ord a) => String -> TwoThreeTree a -> a -> Maybe String
searchSubtree _ EmptyTree _ = Nothing
searchSubtree path (TwoNode v l r) x
  | x == v    = Just path
  | x <  v    = searchSubtree ('L':path) l x
  | otherwise = searchSubtree ('R':path) r x
searchSubtree path (ThreeNode v v' l m r) x
  | x == v || x == v' = Just path
  | x <  v            = searchSubtree ('L':path) l x
  | x <  v'           = searchSubtree ('M':path) m x
  | otherwise         = searchSubtree ('R':path) r x

searchTree :: (Ord a) => TwoThreeTree a -> a -> Maybe String
searchTree tree x = reverse <$> searchSubtree [] tree x

{-
TwoNode 'M' ( ThreeNode 'E' 'J' (ThreeNode 'A' 'C' EmptyTree EmptyTree EmptyTree) (TwoNode 'H' EmptyTree EmptyTree) (TwoNode 'L' EmptyTree EmptyTree) ) ( TwoNode 'R' (TwoNode 'P' EmptyTree EmptyTree) (ThreeNode 'S' 'X' EmptyTree EmptyTree EmptyTree) )
-}
