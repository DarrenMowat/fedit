module FOUL.Parser where

import FOUL.Language

import Control.Monad
import Control.Applicative hiding (many, optional, (<|>))
import Text.Parsec hiding (Line, Empty)
import Text.Parsec.String (Parser)
import Data.Char
import Data.List

{-
  Parser is tasked with taking a string of program text and turning into 
	FOUL Language, which can then be interpreted by the 
	Interpreter module.

  This is loosely based on Prac4.hs but I've replaced the Parser with Parsec as
  I've had a bit of experience using it and it has a cool name.
-}

variable :: Parser String
variable = (:) <$> lower <*> many alphaNum <?> "a variable"

applicator :: Parser String 
applicator = (:) <$> lower <*> many alphaNum <?> "an applicator"

constructor :: Parser String
constructor = (:) <$> upper <*> many alphaNum <?> "a constructor"

inParenthesis :: Parser a -> Parser a
inParenthesis p = between (spaces >> string "(") (spaces >> string ")") p <?> "parenthesis"

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy (spaces >> p <* spaces) (char ',') <?> "comma seperated list" -- The second half of this could ignore everything until it reaches ,

parenList :: Parser a -> Parser [a]
parenList p = spaces >> inParenthesis (commaSep p) <?> "comma seperated list in parenthesis"

{--- 
data Expr
  = EC String [Expr]  -- should look like    Con(e1,...,en)
  | EV String        -- should look like    x
  | EA String [Expr]  -- should look like    f(e1,...en)
  deriving Show
----}

parseExpr :: Parser Expr
parseExpr = spaces >> choice [try parseEA, try parseIntExpr, try parseEC, try parseES, try parseEV] 

parseIntExpr :: Parser Expr
parseIntExpr = mkSucExpr <$> (read :: String -> Integer) <$> many1 digit <?> "an integer"

-- The parser doesn't accept negative (-) numbers so we don't
-- Have to worry about going below Zero here
mkSucExpr :: Integer -> Expr
mkSucExpr 0 = EC "Z" []
mkSucExpr n = EC "S" [mkSucExpr (n - 1)]

parseEC :: Parser Expr
parseEC = EC <$> constructor <*> parenList parseExpr

parseES :: Parser Expr 
parseES = spcCons <$> constructor 

spcCons :: String -> Expr
spcCons n   = EC n []

parseEV :: Parser Expr
parseEV = EV <$> variable 

parseEA :: Parser Expr
parseEA = EA <$> applicator <*> parenList parseExpr

{-
data Pat
  = PC String [Pat]  -- should look like    Con(e1,...,en)
  | PV String        -- should look like    x
  deriving Show
-}

parsePat :: Parser Pat
parsePat = spaces >> choice [try parseIntPat, try parseUnamedVariable, try parsePV, try parsePC, try parsePS]

parseUnamedVariable :: Parser Pat
parseUnamedVariable = PV <$> string "_" <?> "an unamed variable"

parseIntPat :: Parser Pat
parseIntPat = mkSucPat <$> (read :: String -> Integer) <$> many1 digit <?> "an integer"

-- The parser doesn't accept negative (-) numbers so we don't
-- Have to worry about going below Zero here
mkSucPat :: Integer -> Pat
mkSucPat 0 = PC "Z" []
mkSucPat n = PC "S" [mkSucPat (n - 1)]

parsePV :: Parser Pat
parsePV = PV <$> variable <?> "end of input"

parsePC :: Parser Pat
parsePC = PC <$> constructor <*> parenList parsePat

parsePS :: Parser Pat
parsePS = spcPat <$> constructor

spcPat :: String -> Pat
spcPat n   = PC n []

{-
  data Line = Line [Pat] Expr
-}

parseLine :: Parser Line
parseLine = spaces >> parseExpressionLine

parseExpressionLine :: Parser Line
parseExpressionLine =  (,) <$> parenList parsePat  <* spaces <* char '=' <* spaces <*> parseExpr

parseProg :: Parser Prog
parseProg = manyTill parseFunction (try (choice [spaces >> eof, eof]))

parseFunction :: Parser (String, [Line])
parseFunction = spaces >> (,) <$> variable <*> parseLines

parseLines :: Parser [Line]
parseLines = pure (:[]) <*> parseLine 

collateFunctions :: Prog -> Prog -> Prog
collateFunctions []     prog = prog
collateFunctions (x:xs) prog = collateFunctions xs (insertFunction x prog)

insertFunction :: (String, [Line]) -> Prog -> Prog
insertFunction (fname, lns) []                  = [(fname, lns)]
insertFunction (fname, lns) ((fname2, lns2):xs) = if fname == fname2
                                                    then ((fname2, (lns2 ++ lns)) : xs)
                                                  else [(fname2, lns2)] ++ insertFunction (fname, lns) xs

-- Function to strip things that the parser isn't concerned with (imports & comments)
-- Lines are replaced with empty lines to preserve line numbers in error messages
strip :: String -> String 
strip s = unlines $ clearLine "--" $ clearLine "import " $ lines s

clearLine :: String -> [String] -> [String]
clearLine _ []     = [] 
clearLine y (x:xs) = case isPrefixOf y x of 
  True  -> "" : clearLine y xs
  False -> x : clearLine y xs

parseProgram :: String -> Either ParseError Prog
parseProgram prog = case parse parseProg "ParseProgram" (strip prog) of 
  Left er -> Left er
  Right x -> Right (collateFunctions x [])

getImports :: [String] -> [String] 
getImports []     = []
getImports (x:xs) = case stripPrefix "import " x of 
  Just x  -> x : getImports xs
  Nothing -> getImports xs

-- Sanity Checkers
-- Don't want to be interpreting insanely written code

checkProgram :: Prog -> Either String Prog
checkProgram prog = case checkForDuplicateMethods prog [] of 
  []  -> case checkMethodParamaters prog of 
    [] -> Right prog
    xs -> Left (intercalate ", " xs)
  xs  -> Left ("Duplicate method definitions: " ++ (show xs))

checkForDuplicateMethods :: Prog -> [String] -> [String]
checkForDuplicateMethods [] fs            = []
checkForDuplicateMethods ((f, _) : ps) fs = case elem f fs of 
  True  -> f : checkForDuplicateMethods ps fs 
  False -> checkForDuplicateMethods ps (f:fs)

checkMethodParamaters :: Prog -> [String]
checkMethodParamaters []             = []
checkMethodParamaters ((f, ls) : ps) = case group (map (length . fst) ls) of 
  [x] -> checkMethodParamaters ps 
  xs  -> ("Declarations for " ++ f ++ " have diffrent sized paramater lists") : checkMethodParamaters ps


-- checkVariableNameOverwriing :: Prog -> [String] 
-- checkVariableNameOverwriing f