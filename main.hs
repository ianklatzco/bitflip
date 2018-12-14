import System.IO
import Control.Monad

import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)

import Lib
import Parser

read' :: IO String
read' = putStr "flipREPL> "
     >> hFlush stdout
     >> getLine

eval' :: String -> String
eval' input = input

print' :: String -> IO ()
print' = putStrLn

-- main :: IO ()
-- main = do
--   input <- read'

--   unless (input == "quit")
--        $ print' (eval' input) >> main

main :: IO ()
main = do
  putStrLn "hi dan!"
  repl H.empty

repl :: Env -> IO ()
repl env =
  do putStr "flip REPL> "
     hFlush stdout
     input <- getLine
     return ()

     -- case parse atom "stdin" input of
     --    Right QuitStmt -> do putStrLn "Bye!"
     --                         return ()
        -- Right x -> let (newresult,newenv) = exec x penv env
        --            in do {
        --              putStrLn newresult;
        --              repl newenv []
        --            }
        -- Left x -> do putStrLn $ show x
        --              repl penv env [] "stdin"

