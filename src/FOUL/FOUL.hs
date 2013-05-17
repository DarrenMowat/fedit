module FOUL.FOUL(parseToFoul, evalMain, evalExpr) where

import FOUL.ImportResolver
import FOUL.Language
import FOUL.Parser
import Util.EitherUtils
import Data.List (intercalate)
import System.Environment (getArgs)

{-
	FOUL.FOUL is a completely standalone interpreter for FOUL
	It has no dependencies on the fedit editor

	The code below allows this to be compiled into the standalone 
	FOUL interpreter. 
	Just need change this to be the Main module of the program

	Cabal is still required to build this due to the way the 
	FOUL import system accesses standard library modules
-}

parseToFoul :: FilePath -> IO (Either String Prog)
parseToFoul f = do 
  deps <- getDependencies f
  case deps of 
    Left  x    -> return $ Left x
    Right deps -> do 
      let progs = map (parseProgram . snd) deps
      case hasLeft progs of 
        True  -> return $ Left (intercalate ", " (map show (collateLeft progs)))
        False -> return $ checkProgram $ concat (collateRight progs)

evalMain :: Prog -> Either String Val
evalMain prog = evalExpr prog (EA "main" [])

evalExpr :: Prog -> Expr -> Either String Val 
evalExpr p e = eval p [] e

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

