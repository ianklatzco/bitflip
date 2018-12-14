-- Here is a compile-able example of code. I would like to know if there is a "proper" way to lift flip_bits.


import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)

type Env  = H.HashMap String Val

data Val = IntVal Int
         | BoolVal Bool
         | BinListVal String
         | ExceptionVal String
    deriving (Show, Eq)

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


flip_bits :: String -> String
flip_bits [] = []
flip_bits (x:xs) = 
    if x == '1' then
        '0' : flip_bits xs
    else
        '1' : flip_bits xs

eval :: Exp -> Env -> Val
eval (FlipExp f) env =
    let v1 = H.lookup f env in
        case v1 of
            -- The following line seems silly. I feel like I should be able to lift flip_bits to work on BinListVals somehow.
            Just (BinListVal v1) -> BinListVal (flip_bits v1)
            Nothing -> ExceptionVal "No match in env"


-- tests (copy into ghci):
-- h = H.fromList([("h", BinListVal "101" )])
-- eval (FlipExp "h") h -- returns 010

-- 05:06 < ian5v> I have a function String -> String, but i'm curious if i can lift it to work on my own
--                "Val" type.
-- 05:06 < ian5v> https://hastebin.com/cupenohuqi.sql
-- 05:07 < monochrom> The simplest way is to hand-write your own.
-- 05:09 < koz_> ian5v: Val is a sum type. What happens when your Val is 'BoolVal False'?
-- 05:09 < ian5v> something like flip_bits_lifted :: Val -> Val?
-- 05:10 < monochrom> That just tells you you can think up good names.
-- 05:10 < koz_> ian5v: You have a function String -> String.
-- 05:10 < ian5v> koz_: i am unsure! i would guess a runtime exception (something like "nonexhaustive
--                case")
-- 05:10 < sproingie> perhaps you want Val to be a functor
-- 05:11 < ian5v> that sounds right, yeah
-- 05:11 < geekosaur> ian5v, it's a common error to think that Functor meas "liftable". It only means
--                    that under specific circumstances, which don't appear to be met here.
-- 05:11 < monochrom> That is a rabbit hole. Now you want Val to take a type parameter too.
-- 05:11 < monochrom> I still stand by my answer.
-- 05:11 < koz_> Yeah, because it doesn't make sense to have Val take a type parameter, since it's just
--               a tagged value of a set of specific types, in this case {Bool, String, Int}.
-- 05:12 < koz_> What would 'Val Double' even be?
-- 05:13 < ian5v> geekosaur: what are those circumstances?
-- 05:15 < geekosaur> the essence of Functor is fmap :: (a -> b) -> (f a -> f b). This requires that a
--                    can be different from b (hence requiring a type parameter), and that the result be
--                    defined for all inputs provided that the mapped function is
-- 05:17 < geekosaur> in this case, you want f to be Val, but I cannot speak of a Val a or Val b, only
--                    of Val
-- 05:19 < geekosaur> and even if you change Val to fit that, its fmap can't check the type of the
--                    lifted function to see what part of the Val to operate on. Functor is the wrong
--                    tool, in other words
-- 05:20 < ian5v> i see. thank you geekosaur!