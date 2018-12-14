module Parser where

import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity

import Data.Char
import Data.Digits (digits, unDigits)
import Data.Bits

import Lib
import Util

--- Parser
--- ------

-- Pretty name for Parser types
type Parser = ParsecT String () Identity

-- for testing a parser directly
run :: Parser a -> String -> a
run p s     =
    case parse p "<stdin>" s of
        Right x -> x
        Left x  -> error $ show x

-- Lexicals
-- I don't really understand what these are.

symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

-- when run through parse, will return an int.
-- kinda an argument to the parse function
int :: Parser Int
int = do digits <- many1 digit <?> "an integer" -- right side of <?> is error msg
         spaces
         return (read digits :: Int)

-- returns [1,2,0,0,0,1]
binlist :: Parser [Int]
binlist = do _ <- string "0b"
             all_digits <- many1 digit <?> "oopsie woopsie"
             return $ digits 10 (read all_digits :: Int)

-- Expressions

-- when runs thru parse, returns an int exp?
intExp :: Parser Exp
intExp = do i <- int -- this is the int function from the above lexicals section.
            return $ IntExp i

boolExp :: Parser Exp
boolExp =    ( symbol "true"  >> return (BoolExp True)  )
         <|> ( symbol "false" >> return (BoolExp False) )

flipExp :: Parser Exp
flipExp = do 
            try $ symbol "(ノ ゜Д゜)ノ ︵"
            return $ FlipExp "hello" 3

-- (<|>) :: (Alternative f) => f a -> f a -> f a
-- it's an operator from Parsec, means "try any of these!"
atom :: Parser Exp
atom = intExp
   <|> try boolExp -- why is the try necessary?
   <|> flipExp

-- case parse atom "stdin" input of
--     Right x -> 

--         Right x -> let (nuresult,nupenv,nuenv) = exec x penv env
--                    in do {
--                      putStrLn nuresult;
--                      repl nupenv nuenv [] "stdin"
--                    }
--         Left x -> do putStrLn $ show x
--                      repl penv env [] "stdin"

