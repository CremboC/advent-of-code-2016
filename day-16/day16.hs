module Main where

import Data.List (find)

input :: String
input = "10010000000110000"

example :: String
example = "10000"

dragon :: String -> String
dragon s = s ++ "0" ++ (map repl . reverse $ s)
    where 
        repl '1' = '0'
        repl '0' = '1'
        repl c = c

dragon' :: String -> [String]
dragon' s = s' : (dragon' s')
    where s' = dragon s

unpair :: String -> String
unpair (a:b:xs) = (if a == b then '1' else '0') : unpair xs
unpair _ = []

unpair' :: String -> [String]
unpair' s = s' : (unpair' s')
    where s' = unpair s

checksum :: Int -> String -> Maybe String
checksum size s = find f . unpair' . take size $ s
    where f x = odd . length $ x

main :: IO ()
main = do
    let f t x = (>= t) . length $ x

    -- example
    print $ (>>= (checksum 20)) . find (f 20) . dragon' $ example

    -- part 1
    print $ (>>= (checksum 272)) . find (f 272) . dragon' $ input

    -- part 2
    print $ (>>= (checksum 35651584)) . find (f 35651584) . dragon' $ input