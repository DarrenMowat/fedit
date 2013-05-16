module FOUL.FOULLang where

import FOUL.ImportResolver
import FOUL.Language
import FOUL.Parser
import Util.EitherUtils
import Data.List (intercalate)

parseToFoul :: FilePath -> IO (Either String Prog)
parseToFoul f = do 
  deps <- getDependencies f
  case deps of 
    Left  x    -> return $ Left x
    Right deps -> do 
      -- putStrLn (show deps)
      let progs = map (parseProgram . snd) deps
      -- putStrLn (show progs)
      case hasLeft progs of 
        True  -> return $ Left (intercalate ", " (collateLeft progs))
        False -> return $ Right (concat (collateRight progs))

evalMain :: Prog -> Val
evalMain prog = evalExpr prog (EA "main" [])

evalExpr :: Prog -> Expr -> Val 
evalExpr p e = eval p [] e