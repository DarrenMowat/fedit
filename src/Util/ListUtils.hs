module Util.ListUtils where

import Data.Char(isSpace)

{-|
  Functions to trim stuff from the head and tail of lists

  Made generic from https://code.google.com/p/haskellnotebook/wiki/HaskellTrimString
-}
trim :: (a -> Bool) -> [a] -> [a]
trim x = f . f where f = reverse . dropWhile x

trimString :: String -> String
trimString = trim isSpace
