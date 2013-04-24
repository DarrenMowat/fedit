module FOUL.Language where

import Debug.Trace

{-
A FOUL program consists of a bunch of named functions, each of which
is defined by some program lines.
-}

type FName = String
type Prog = [(FName, [Line])]

{-
A function line has a bunch of patterns on the left, and an expression
to evaluate if those patterns match the function's inputs.
-}

-- type Line = ([Pat], Expr)

data Line = Line [Pat] Expr | Comment String | Empty deriving (Show, Eq)



{-******************************************************************-}
{- FOUL Values                                                      -}
{-******************************************************************-}

type CName = String
data Val = VC CName [Val]  -- a constructor with 0 or more subvalues
  deriving (Show, Eq)

true :: Val
true = VC "True" []

false :: Val
false = VC "False" []

zero :: Val
zero = VC "Zero" []  -- zero has no subvalues

suc :: Val -> Val
suc n = VC "Suc" [n]

nil :: Val
nil = VC "[]" []

cons :: Val -> Val -> Val
cons x xs = VC ":" [x, xs]

pair :: Val -> Val -> Val
pair a b = VC "Pair" [a, b]

leaf :: Val
leaf = VC "Leaf" []

node :: Val -> Val -> Val -> Val
node left label right = VC "Node" [left, label, right]

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

{- Expressions are built as follows -}

data Expr
  = EC CName [Expr]   -- just like values
  | EV VName          -- from variables (coming from patterns)
  | EA FName [Expr]   -- by applying functions (from the program)
  deriving (Show, Eq)

{-
We'd better check that we can make constant expressions from values.
-}

constant :: Val -> Expr
constant (VC c vs) = EC c (map constant vs)

{-
Let's evaluate expressions: we'll need a program to interpret
functions and an environment to interpret variables.
-}

eval :: Prog -> Env -> Expr -> Val
eval fs gam (EC c es)  = VC c (map (eval fs gam) es)
eval fs gam (EV x)     = fetch x gam
eval fs gam (EA f es)  = runfun (fetch f fs) (map (eval fs gam) es)
  where
    runfun :: [Line] -> [Val] -> Val
    runfun ((Comment _) : ls) vs = runfun ls vs
    runfun (Empty : ls) vs = runfun ls vs
    runfun ((Line ps e) : ls) vs = case matches ps vs of
      Nothing    -> runfun ls vs
      Just gam'  -> eval fs gam' e

{- We need that looker-upper function. -}

fetch :: String -> [(String, x)] -> x
fetch _ [] = undefined -- Need a way to throw an error that x is undefined in the program file. Could be an undefined program etc. Either x String, Maybe x, the caller can then propogate the message
fetch x ((y, v) : gam)
  | x == y     = v
  | otherwise  = fetch x gam
