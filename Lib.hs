module Lib where

import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)

type Env  = H.HashMap String Val

--- ### Values
data Val = IntVal Int
         | BoolVal Bool
         | BinListVal [Int]
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (BinListVal xs) = concat $ map show xs

--- ### Expressions
data Exp = IntExp Int
         | BoolExp Bool
         -- | LetExp [(String,Exp)] Exp
         | FlipExp String Int
    deriving (Show, Eq)

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i
