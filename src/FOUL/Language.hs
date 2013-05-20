module FOUL.Language where

import Debug.Trace
import Util.EitherUtils
import Data.List

{-
A FOUL program consists of a bunch of named functions, each of which
is defined by some program lines.
-}

type FName = String
type Prog = [(FName, [Line])]

{-
A function line has a bunch of patterns on the left, and an expression
to evaluate if those patterns match the function's inputs.
  Lines can also be comments or empty
-}

type Line = ([Pat],Expr)


{-******************************************************************-}
{- FOUL Values                                                      -}
{-******************************************************************-}

type CName = String
data Val = VC CName [Val]  -- a constructor with 0 or more subvalues
  deriving (Eq)

-- This show instance makes values pretty for printing
instance Show Val where 
  show = prettyPrintVal

prettyPrintVal :: Val -> String 
prettyPrintVal (VC c vs) = case sucToInt (VC c vs) of 
  Right i -> (show i)
  Left _  -> case null vs of 
    True  -> c
    False -> concat $ [c, " (", intercalate ", " vvs, ")"]
  where 
    vvs = map prettyPrintVal vs

sucToInt :: Val -> Either Val Int
sucToInt (VC "Z" _)  = Right 0
sucToInt (VC "S" xs) = case hasLeft xxs of 
  True  -> Left (VC "S" xs)
  False -> Right $ 1 + sum (collateRight xxs)
  where
    xxs = map sucToInt xs
sucToInt v = Left v

{-******************************************************************-}
{- FOUL Patterns                                                    -}
{-******************************************************************-}

{-
A pattern is like a value, except that some parts have been abstracted
away by variables. That is, a pattern is a value template.
-}

type VName = String
data Pat
  =  PV VName
  |  PC CName [Pat]
  deriving (Show, Eq)

{-
We can try to figure out if a value matches a pattern. If so, we can
build an environment showing how the pattern variables correspond to
pieces of the value matched.
-}

type Env = [(VName, Val)]

{- Let's write a matching algorithm. -}

match :: Pat -> Val -> Maybe Env
match (PV x) v = Just [(x, v)]
match (PC c ps) (VC c' vs)
  | c == c'    = matches ps vs
  | otherwise  = Nothing

matches :: [Pat] -> [Val] -> Maybe Env
matches [] [] = Just []
matches (p : ps) (v : vs) = case match p v of
  Nothing -> Nothing
  Just e1 -> case matches ps vs of
    Nothing -> Nothing
    Just e2 -> Just (e1 ++ e2)
matches _ _ = Nothing


{-******************************************************************-}
{- FOUL Expressions                                                 -}
{-******************************************************************-}

data Expr
  = EC CName [Expr]   -- just like values
  | EV VName          -- from variables (coming from patterns)
  | EA FName [Expr]   -- by applying functions (from the program)
  deriving (Show, Eq)

constant :: Val -> Expr
constant (VC c vs) = EC c (map constant vs)

{-
Let's evaluate expressions: we'll need a program to interpret
functions and an environment to interpret variables.
-}

eval :: Prog -> Env -> Expr -> Either String Val
eval fs gam (EC c es)  = case hasLeft vals of 
  True  -> Left $ intercalate ", " $ nub $ collateLeft vals
  False -> Right $ VC c (collateRight vals)
  where    
    vals :: [Either String Val]
    vals = map (eval fs gam) es
eval fs gam (EV x)     = case fetch x gam of 
  Nothing -> Left $ "Couldn't find variable " ++ x 
  Just x -> Right x
eval fs gam (EA f es)  = case fetch f fs of 
  Nothing -> Left $ "Couldn't find function " ++ f
  Just fn -> case hasLeft vals of 
    True  -> Left $ intercalate ", " $ nub $ collateLeft vals
    False -> runfun f fn (collateRight vals)
  where 
    vals :: [Either String Val]
    vals = map (eval fs gam) es
    runfun :: String -> [Line] -> [Val] -> Either String Val
    runfun f ((ps, e) : ls) vs = case matches ps vs of
      Nothing    -> runfun f ls vs
      Just gam'  -> eval fs gam' e
    runfun f _ _ = Left $ "Non-exhaustive pattern encountered in function " ++ f

{- We need that looker-upper function. -}
{- Wrapped in Maybe to protect against things we need to find but don't have (Mistyped/Missing function names and variables) -}

fetch :: String -> [(String, x)] -> Maybe x
fetch x [] = Nothing
fetch x ((y, v) : gam)
  | x == y     = Just v
  | otherwise  = fetch x gam