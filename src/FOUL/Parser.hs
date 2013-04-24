module FOUL.Parser where

import FOUL.Language

import Control.Monad
import Control.Applicative hiding (many, optional, (<|>))
import Text.Parsec hiding (Line, Empty)
import Text.Parsec.String (Parser) -- type Parser = Parsec String ()
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellStyle)
import Data.Char

{-
	
  Parser is tasked with taking a string of program text and turning into 
	FOUL Intermediate Language, which can then be interpreted by the 
	Interpreter module.

  This is based on Prac4.hs but I've replaced the Parser with Parsec as
  I've had a bit of experience using it and its pretty cool.

-}



run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "run" input) of
  Left err -> do 
    putStr "parse error at " 
    print err
    print $ errorPos err
  Right x -> print x

variable :: Parser String
variable = (:) <$> lower <*> many alphaNum <?> "a variable"

applicator :: Parser String 
applicator = (:) <$> lower <*> many alphaNum <?> "an applicator"

constructor :: Parser String
constructor = (:) <$> upper <*> many alphaNum <?> "Expected an constructor"

inParenthesis :: Parser a -> Parser a
inParenthesis p = between (spaces >> string "(") (spaces >> string ")") p <?> "Expected parenthesis"

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy (spaces >> p <* spaces) (char ',') <?> "Expected comma seperated list" -- The second half of this could ignore everything until it reaches ,

parenList :: Parser a -> Parser [a]
parenList p = spaces >> inParenthesis (commaSep p) <?> "Expected comma seperated list in parenthesis"

integer :: Parser Integer
integer = (read :: String -> Integer) <$> many1 digit <?> "Expected an integer"

keywordImport :: Parser ()
keywordImport  = try (do{ string "import" 
                       ; notFollowedBy alphaNum
                       }) <?> "Expected import keyword"

{--- remember
data Expr
  = EC String [Expr]  -- should look like    Con(e1,...,en)
  | EV String        -- should look like    x
  | EA String [Expr]  -- should look like    f(e1,...en)
  deriving Show
----}

parseExpr :: Parser Expr
parseExpr = spaces >> choice [try parseEA, try parseEC, try parseES, try parseEV] 

parseEC :: Parser Expr
parseEC = EC <$> constructor <*> parenList parseExpr

parseES :: Parser Expr 
parseES = spcCons <$> constructor 

spcCons :: String -> Expr
spcCons "Z" = EC "Zero" []
spcCons "T" = EC "True" []
spcCons "F" = EC "False" []
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
parsePat = spaces >> choice [try parsePV, try parsePC, try parsePS]

parsePV :: Parser Pat
parsePV = PV <$> variable <?> "end of input"

parsePC :: Parser Pat
parsePC = PC <$> constructor <*> parenList parsePat

parsePS :: Parser Pat
parsePS = spcPat <$> constructor

spcPat :: String -> Pat
spcPat "Z" = PC "Zero" []
spcPat "T" = PC "True" []
spcPat "F" = PC "False" []
spcPat n   = PC n []

{-
  data Line = Line [Pat] Expr | Comment String | Empty
-}

parseLine :: Parser Line
parseLine = spaces >> choice [try parseCommentLine, try parseExpressionLine]

parseCommentLine :: Parser Line
parseCommentLine = string "--" >> Comment <$> manyTill anyChar (try newline) 

parseExpressionLine :: Parser Line
parseExpressionLine =  Line <$> parenList parsePat  <* spaces <* char '=' <* spaces <*> parseExpr

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

parseProgram :: String -> Either String Prog
parseProgram prog = case parse parseProg "ParseProgram" prog of 
  Left err -> Left ((show err) ++ " -> " ++ (show $ errorPos err)) 
  Right x -> Right (collateFunctions x [])






