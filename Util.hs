module Util where

import Data.Digits (digits, unDigits)

-- https://stackoverflow.com/a/10028866
convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase from to = digits to . unDigits from

-- https://stackoverflow.com/a/44218722
binary_convert :: [Int] -> Int
binary_convert [] = 0
binary_convert (x : xs) = x + 2 * binary_convert xs

flip_bits :: String -> String
flip_bits [] = []
flip_bits (x:xs) = 
    if x == '1' then
        '0' : flip_bits xs
    else
        '1' : flip_bits xs