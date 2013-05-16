module FOUL.ImportResolver (getDependencies) where

import FOUL.Parser
import Util.FileUtils (splitPath)
import Util.ListUtils (trimString)
import Util.EitherUtils
import System.FilePath ((</>))
import Data.List (isSuffixOf, intercalate, nub)
import Paths_fedit (getDataFileName)

{-
  FOUL.ImportResolver deals with figuring out which modules
  need to be loaded to successfully interpret the program

-}

-- Mapping of standard modules names to filenames
-- These can then be accessed via cabals getDataFileName
stdLib :: [(String, String)]
stdLib = [("std/bool", "std/bool.foul"),
          ("std/math", "std/math.foul")]

data ImportTree = Branch (FilePath, String) [ImportTree] | Leaf (FilePath, String)

{-
  Return a list, and their content, that are required to interpret the program.
  Modules are searched for imports recursively so your imports can have imports, 

  like a dream within a dream... INCEPTION

  We return the FilePath & contents of each modules so they are only 
  loaded from the disk once. 
-}
getDependencies :: FilePath -> IO (Either String [(FilePath, String)])
getDependencies f = do 
  tree <- makeImportTree [] f
  case tree of 
  	Left e  -> return (Left e)
  	Right t -> return (Right $ nub $ flattenImportTree t) -- We use nub here to get rid of modules that may have been loaded may times by other modules

{-
  Generate an import tree for the given file
  We don't support cyclic imports, if we did we'd loop forever... 
  This function checks for cycles and complains if there is one (or many)
  Otherwise we return an ImportTree 
-}
makeImportTree :: [FilePath] -> FilePath -> IO (Either String ImportTree)
makeImportTree past root = do 
  contents <- readFile root
  let imps = nub $ getImports (lines contents) -- Incase they import the same module twice, save some work
  ifs <- mapM (getImportPath root) imps
  case elem root past of 
    True  -> return $ Left ("Cyclic import detected! " ++ root ++ " imported by " ++ (head past))
    False -> case ifs of 
      [] -> return $ Right (Leaf (root, contents))
      otherwise -> do
        mfs <- mapM (makeImportTree (root : past)) ifs
        case hasLeft mfs of 
          True  -> return $ Left (intercalate "," (collateLeft mfs)) 
          False -> return $ Right (Branch (root, contents) (collateRight mfs))

{-
  Get the path on the filesystem of the module. 
  This can be a standard library module or a user defined module
-}
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