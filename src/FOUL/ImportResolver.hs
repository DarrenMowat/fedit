module FOUL.ImportResolver (getDependencies) where

import FOUL.Parser
import Util.FileUtils (splitPath)
import Util.ListUtils (trimString)
import Util.EitherUtils
import System.FilePath ((</>))
import System.Directory (doesFileExist, getPermissions, Permissions(..))
import Data.List (isSuffixOf, intercalate, nub)
import Paths_fedit (getDataFileName)

{-
  FOUL.ImportResolver deals with figuring out which modules
  need to be loaded to successfully interpret the program

  TODO: Allow user to choose which functions to import from a module

-}

-- Mapping of standard modules names to filenames
-- These can then be accessed via cabals getDataFileName
stdLib :: [(String, String)]
stdLib = [("std/bool", "std/bool.foul"),
          ("std/test", "std/test.foul"),
          ("std/math", "std/math.foul")]

type ModuleFile = (FilePath, String)

data ImportTree = Branch (ModuleFile, String) [ImportTree] | Leaf (ModuleFile, String)

{-
  Return a list, and their content, that are required to interpret the program.
  Modules are searched for imports recursively so your imports can have imports, 

  like a dream within a dream... INCEPTION

  We return the FilePath & contents of each modules so they are only 
  loaded from the disk once. 
-}
getDependencies :: FilePath -> IO (Either String [(ModuleFile, String)])
getDependencies f = do 
  tree <- makeImportTree [] ("main_module", f)
  case tree of 
  	Left e  -> return (Left e)
    -- We use nub here to get rid of modules that may have been loaded many times by other modules
    -- This is inefficient as we may have loaded the same module tree many times
    -- However it is essential in checking for cycles. Could reimplement makeImportTree to hold some
    -- sort of state of whats already been loaded in all paths, not just the current path of the tree
  	Right t -> return (Right $ nub $ flattenImportTree t) 

{-
  Generate an import tree for the given file
  We don't support cyclic imports, if we did we could potentially loop forever... 
  This function checks for cycles and complains if there is one (or many - We detect all cyclic dependencies in one shot)
  Otherwise we return an ImportTree 
-}
makeImportTree :: [ModuleFile] -> ModuleFile -> IO (Either String ImportTree)
makeImportTree past (name, root) = do 
  exists <- doesFileExist root
  case exists of 
    False -> return $ Left ("Module " ++ name ++ " does not exist [" ++ root ++ "]")
    True  -> do 
      permissions <- getPermissions root
      case readable permissions of 
        False -> return $ Left ("Module " ++ name ++ " isn't readable. Ensure the current user has read permissions for the file. [" ++ root ++ "]")
        True  -> do
          contents <- readFile root
          let imps = nub $ getImports (lines contents) -- Incase they import the same module twice
          ifs <- mapM (getImportPath root) imps
          case elem (name, root) past of 
            True  -> return $ Left ("Cyclic import detected! " ++ name ++ " imported by " ++ (snd $ head past))
            False -> case ifs of 
              [] -> return $ Right (Leaf ((root, name), contents))
              otherwise -> do
                mfs <- mapM (makeImportTree ((name, root) : past)) ifs
                case hasLeft mfs of 
                  True  -> return $ Left (intercalate "," (collateLeft mfs)) 
                  False -> return $ Right (Branch ((root, name), contents) (collateRight mfs))

{-
  Get the path on the filesystem of the module. 
  This can be a standard library module or a user defined module
-}
getImportPath :: FilePath -> String -> IO ModuleFile
getImportPath root name = case lookup (trimString name) stdLib of 
  Just i  -> do 
    f <- getDataFileName i
    return (name, f)
  Nothing -> do 
    let (d, p) = splitPath (trimString root)
    let ip = d </> (trimString name)
    case isSuffixOf ".foul" ip of 
      True  -> return (name, ip)
      False -> return (name, (ip ++ ".foul"))

flattenImportTree :: ImportTree -> [(ModuleFile, String)]
flattenImportTree (Leaf x) = [x]
flattenImportTree (Branch x zs) = [x] ++ (concat $ map flattenImportTree zs)