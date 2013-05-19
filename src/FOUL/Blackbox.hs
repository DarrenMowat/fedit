module FOUL.Blackbox where

import FOUL.FOUL
import FOUL.Language

buildEvalList :: [String] -> [Expr] 
buildEvalList [] = []
buildEvalList (x:xs) = buildEvalList xs