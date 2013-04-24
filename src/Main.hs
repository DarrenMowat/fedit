module Main where

import System.Environment (getArgs)

import Fedit
import FOUL.FOULLang

main :: IO ()
main = do
  args <- getArgs
  case args of 
  	[] -> putStrLn "No input specified"
  	xs -> do 
  		contents <- readFile $ concat xs
  		parseProgram contents

parseProgram :: String -> IO ()
parseProgram contents = do
  case parseAndEval contents of
    Left err -> putStrLn $ "ERROR: " ++ err
    Right suc -> putStrLn $ "SUCCSESS: " ++ suc