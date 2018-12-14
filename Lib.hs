module Lib where

-- Types

import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)

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

--- ### Expressions
data Exp = IntExp Int
         | BoolExp Bool
         | BinListExp String
         | VarExp String
         | AssignmentExp String Exp
         | ReassignmentExp String Exp
         | FlipExp String
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


--- Exec
--- ----

-- Variable Assignment
exec :: Exp -> Env -> Result
exec = undefined
-- exec (AssignmentExp varstring intlist) env = ("", H.insert varstring intlist env)

-- exec (ReassignmentExp varstring expr) env =
--     ("", H.insert varstring )
--     eval expr env

