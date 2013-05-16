module Util.EitherUtils where

{-
  Simple utils for collating groups of
  Lefts & Rights together
-}

hasLeft :: [Either a b] -> Bool
hasLeft xs = case collateLeft xs of 
	[] -> False 
	_  -> True

collateLeft :: [Either a b] -> [a]
collateLeft [] = [] 
collateLeft ((Left l) : ls) = l : collateLeft ls
collateLeft (l : ls) = collateLeft ls

hasRight :: [Either a b] -> Bool
hasRight xs = case collateRight xs of 
	[] -> False
	_  -> True

collateRight :: [Either a b] -> [b]
collateRight [] = [] 
collateRight ((Right r) : rs) = r : collateRight rs
collateRight (r : rs) = collateRight rs