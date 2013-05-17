module Main where

import System.Environment (getArgs)

import Fedit
import FOUL.FOUL

main :: IO ()
main = do
  args <- getArgs
  case args of 
  	[] -> putStrLn "No input specified"
  	xs -> parseProgram $ concat xs

parseProgram :: FilePath -> IO ()
parseProgram file = do
  res <- parseToFoul file
  case res of
    Left err -> putStrLn $ "ERROR: " ++ err
    Right prog -> putStrLn $ "Program() -> " ++ (show prog) ++ "\n\nmain() -> " ++ (show $ evalMain prog) ++ "\n"

