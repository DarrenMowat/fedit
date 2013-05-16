module FOUL.ImportResolver where

import FOUL.Parser
import Util.FileUtils (splitPath)
import Util.ListUtils (trimString)
import System.FilePath ((</>))
import Data.List (isSuffixOf)
import Paths_fedit (getDataFileName)

stdLib :: [(String, String)]
stdLib = [("std/bool", "std/bool.foul"),
          ("std/math", "std/math.foul")]

data ImportTree = Branch (FilePath, String) [ImportTree] | Leaf (FilePath, String)

getDependencies :: FilePath -> IO (Either String [(FilePath, String)])
getDependencies f = do 
  tree <- makeImportTree f [] 
  case tree of 
  	Left e  -> return (Left e)
  	Right t -> return (Right (flattenImportTree t))

makeImportTree :: FilePath -> [FilePath] -> IO (Either String ImportTree)
makeImportTree root past = do 
  contents <- readFile root
  let imps = getImports (lines contents)
  ifs <- mapM (getImportPath root) imps
  case ifs of 
    [] -> return $ Right (Leaf (root, contents))
    otherwise -> do 
      return (Left "")

getImportPath :: FilePath -> String -> IO FilePath
getImportPath root name = case lookup (trimString name) stdLib of 
  Just i  -> getDataFileName i
  Nothing -> do 
    let (d, p) = splitPath (trimString root)
    let ip = d </> (trimString name)
    case isSuffixOf ".foul" ip of 
      True  -> return ip
      False -> return (ip ++ ".foul")

flattenImportTree :: ImportTree -> [(FilePath, String)]
flattenImportTree (Leaf x) = [x]
flattenImportTree (Branch x zs) = [x] ++ (concat $ map flattenImportTree zs)