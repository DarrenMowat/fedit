module FOUL.FOULLang where

import FOUL.ImportResolver
import FOUL.Language
import FOUL.Parser

parseToFoul :: FilePath -> IO (Either String Prog)
parseToFoul f = do 
  deps <- getDependencies f
  case deps of 
  	Left  x    -> return $ Left x
  	Right deps -> do 
        contents <- readFile f
        return $ parseProgram contents

evalMain :: Prog -> Val
evalMain prog = evalExpr prog (EA "main" [])

evalExpr :: Prog -> Expr -> Val 
evalExpr p e = eval p [] e