module Lib where

import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)

import Util

type Env  = H.HashMap String Val
type Result = (String, Env)

--- ### Values
data Val = IntVal Int
         | BoolVal Bool
         | BinListVal String
         | ExceptionVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (BinListVal xs) = concat $ map show xs
    show (ExceptionVal s) = show s

--- ### Expressions
data Exp = IntExp Int
         | BoolExp Bool
         | BinListExp String
         | VarExp String
         | AssignmentExp String Exp
         | ReassignmentExp String Exp
         | FlipExp String
         | PrintExp String
    deriving (Show, Eq)

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

eval (VarExp s) env = 
    let v1 = H.lookup s env in
      case v1 of
        Just v1 -> v1
        Nothing -> ExceptionVal "No match in env"

eval (FlipExp f) env =
    let v1 = H.lookup f env in
        case v1 of
            Just (BinListVal v1) -> BinListVal (flip_bits v1)
            Nothing -> ExceptionVal "No match in env"

eval (BinListExp str) env = BinListVal str


--- Exec
--- Exec is a thin wrapper around eval to allow for modification of the env 
--- and returning a string result to be printed.
--- ----

-- Variable Assignment
exec :: Exp -> Env -> Result
exec (AssignmentExp varstring expr) env = 
    ("", H.insert varstring e env)
        where e = eval expr env

exec (ReassignmentExp varstring expr) env =
    ("", H.insert varstring e env)
        where e = eval expr env

exec (PrintExp varstring) env =
    let res = H.lookup varstring env in
        case res of
            Nothing -> ("Not found", env)
            Just (BinListVal x) -> (x++"\n", env)
            Just (IntVal x) -> ((show x)++"\n", env)
            Just (ExceptionVal x) -> ("Exception!: " ++x  ++"\n", env)

