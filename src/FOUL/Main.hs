module Main where

import System.Environment (getArgs)

import FOUL.FOUL

{-
  FOUL.FOUL is a completely standalone interpreter for FOUL
  It has no dependencies on the fedit editor

  The code below allows this to be compiled into the standalone 
  FOUL interpreter. 
  Just need change this to be the Main module of the program

  Cabal is still required to build this due to the way the 
  FOUL import system accesses standard library modules
-}

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [] -> putStrLn "No input specified"
    xs -> go $ concat xs

go :: FilePath -> IO ()
go file = do
  res <- parseToFoul file
  case res of
    Left err -> putStrLn $ "ERROR: " ++ err
    Right prog -> putStrLn $ "\nmain() -> " ++ (show $ evalMain prog) ++ "\n"
