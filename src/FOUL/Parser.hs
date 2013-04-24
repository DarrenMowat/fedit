module FOUL.Parser where

import FOUL.Language

import Control.Monad
import Control.Applicative hiding (many, optional, (<|>))
import Text.Parsec
import Text.Parsec.String (Parser) -- type Parser = Parsec String ()
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellStyle)
import Data.Char

{-
	Parser is tasked with taking a string of program text and turning into 
	FOUL Intermediate Language, which can then be interpreted by the 
	Interpreter module.
-}



run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "run" input) of
  Left err -> do 
    putStr "parse error at " 
    print err
  Right x -> print x

variable :: Parser String
variable = (:) <$> lower <*> many alphaNum

applicator :: Parser String 
applicator = (:) <$> lower <*> many alphaNum 

constructor :: Parser String
constructor = (:) <$> upper <*> many alphaNum

inParenthesis :: Parser a -> Parser a
inParenthesis p = between (spaces >> string "(") (spaces >> string ")") p

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy (spaces >> p) (spaces >> char ',') -- The second half of this could ignore everything until it reaches ,

parenList :: Parser a -> Parser [a]
parenList p = inParenthesis (commaSep p)

integer :: Parser Integer
integer = (read :: String -> Integer) <$> many1 digit

keywordImport :: Parser ()
keywordImport  = try (do{ string "import" 
                       ; notFollowedBy alphaNum
                       })

{--- remember
data Expr
  = EC String [Expr]  -- should look like    Con(e1,...,en)
  | EV String        -- should look like    x
  | EA String [Expr]  -- should look like    f(e1,...en)
  deriving Show
----}

parseExpr :: Parser Expr
parseExpr = spaces >> choice [try parseEA, try parseEC, try parseEV] 

parseEC :: Parser Expr
parseEC = EC <$> constructor <*> parenList parseExpr

parseEV :: Parser Expr
parseEV = EV <$> variable 

parseEA :: Parser Expr
parseEA = EA <$> applicator <*> parenList parseExpr








parseProgram :: String -> Either String Prog
parseProgram prog = undefined



