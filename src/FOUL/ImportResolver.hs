module FOUL.ImportResolver where

import FOUL.Parser
import Util.FileUtils (splitPath)
import Util.ListUtils (trimString)
import System.FilePath ((</>))
import Data.List (isSuffixOf, intercalate)
import Paths_fedit (getDataFileName)

stdLib :: [(String, String)]
stdLib = [("std/bool", "std/bool.foul"),
          ("std/math", "std/math.foul")]

data ImportTree = Branch (FilePath, String) [ImportTree] | Leaf (FilePath, String)

getDependencies :: FilePath -> IO (Either String [(FilePath, String)])
getDependencies f = do 
  tree <- makeImportTree [] f
  case tree of 
  	Left e  -> return (Left e)
  	Right t -> return (Right (flattenImportTree t))

makeImportTree :: [FilePath] -> FilePath -> IO (Either String ImportTree)
makeImportTree past root = do 
  contents <- readFile root
  let imps = getImports (lines contents)
  ifs <- mapM (getImportPath root) imps
  case elem root past of 
    True  -> return $ Left ("Cyclic import detected for " ++ root ++ " in " ++ (head past))
    False -> case ifs of 
      [] -> return $ Right (Leaf (root, contents))
      otherwise -> do
        mfs <- mapM (makeImportTree (root : past)) ifs
        case collateErrors mfs of 
          [] -> return (Right (Branch (root, contents) (collateBranches mfs)))
          es -> return (Left (intercalate ", " es))

collateErrors :: [Either String ImportTree] -> [String]
collateErrors [] = [] 
collateErrors ((Left e) : es) = e : collateErrors es
collateErrors (e : es)        = collateErrors es

collateBranches :: [Either String ImportTree] -> [ImportTree]
collateBranches [] = [] 
collateBranches ((Right i) : is) = i : collateBranches is

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