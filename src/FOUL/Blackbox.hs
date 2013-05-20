module FOUL.Blackbox where


import System.FilePath ((</>))
import System.Directory(removeDirectoryRecursive)
import Text.Parsec hiding (Line, Empty)
import Text.Parsec.String (Parser)

import Data.List

import FOUL.FOUL
import FOUL.Language
import FOUL.Parser
import Util.FileUtils

{-
    This module allows for special eval comments to be automatically 
    evaluated by the caller.

    Expressions like 

        -- eval add(7, 8)

    will be automatically found and evaluated, answers get inserted back in

-}

buildEvalList :: [String] -> [(String, Either String Expr)] 
buildEvalList [] = []
buildEvalList (x:xs) = case isPrefixOf "-- eval " x && not (isInfixOf " -> " x) of 
  False -> buildEvalList xs 
  True  -> case parse parseExpr "ParseExpression" (drop 8 x) of 
    Left er -> (x, Left (show er)) : buildEvalList xs
    Right e -> (x, Right e) : buildEvalList xs

insertValues :: [String] -> [(String, Either String Val)] -> [String]
insertValues [] [] = []
insertValues xs [] = xs
insertValues (x:xs) ((v, e):vs) = case x == v of 
  False -> x : insertValues xs ((v, e):vs)
  True  -> (x ++ " -> " ++ getVal e) : insertValues xs vs
  where
    getVal (Left err) = show err
    getVal (Right val) = show val

--             File        BufferContents      New Buffer
runBlackbox :: FilePath -> String -> IO (String, String)
runBlackbox file buffer = do 
  let (dir, name) = splitPath file
  -- We copy to a new temp directory so we don't break the 
  -- users file 
  twd <- copyToNewTempDir dir
  let tfile = twd </> name
  writeFile tfile buffer
  -- Start some work
  let els = buildEvalList (lines buffer)
  prog <- parseToFoulFromFile tfile
  case prog of 
  	Left err -> do 
  		removeDirectoryRecursive twd
  		return (err, buffer)
  	Right prog -> do 
  		let nbuffer = insertValues (lines buffer) (map (evalProgExpr prog) els)
  		return ("Parsed Successfully", unlines nbuffer)

evalProgExpr :: Prog -> (String, Either String Expr) -> (String, Either String Val)
evalProgExpr prog (s, Left e)  = (s, Left e)
evalProgExpr prog (s, Right e) = (s, evalExpr prog e)