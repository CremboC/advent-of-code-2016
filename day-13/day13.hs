{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Search.AStar (astar)
import Search.BFS (bfs)
import Data.Bits (popCount)

input :: Integer
input = 1352

formula :: (Integer, Integer) -> Integer
formula (x, y) = (x * x) + (3 * x) + (2 * x * y) + (y) + (y * y) + input

isValid :: (Integer, Integer) -> Bool
isValid c@(x, y) = x >= 0 && y >= 0 && (even . popCount . formula $ c)

main :: IO ()
main = do
    let start = (1, 1)
    let target = (31, 39)
    let targeter = (== target)
    let entree (x, y) = filter isValid [(x, succ y), (x, pred y), (succ x, y), (pred x, y)]

    print $ bfs start targeter entree
    -- print $ astar start target isValid
