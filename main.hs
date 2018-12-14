import System.IO
import Control.Monad

import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity

import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)

import Lib
import Parser



main :: IO ()
main = do
  putStrLn "hi dan!"
  repl H.empty

repl :: Env -> IO ()
repl env =
  do putStr "flip REPL> "
     hFlush stdout
     input <- getLine

     if input == "help" then
        do
            putStrLn "https://twitter.com/quietly_turning/status/1073274423766065152"
            repl env
     else
         case parse atom "stdin" input of
         --    Right QuitStmt -> do putStrLn "Bye!"
         --                         return ()
            Right x -> let (newresult,newenv) = exec x env
                       in do {
                         putStr newresult;
                         repl newenv
                       }
            Left x -> do putStrLn $ show x
                         repl env

