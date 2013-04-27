module FOUL.FOULLang where

import FOUL.Language
import FOUL.Parser

parseToFoul :: String -> Either String Prog
parseToFoul []    = Left "Empty Input"
parseToFoul input = parseProgram input

evalMain :: Prog -> Val
evalMain prog = evalExpr prog (EA "main" [])

evalExpr :: Prog -> Expr -> Val 
evalExpr p e = eval p [] e