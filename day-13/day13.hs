{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Search.AStar (astar)
import Search.BFS (bfs)
import Data.Bits (popCount)

type Coord = (Integer, Integer)

input :: Integer
input = 1352

formula :: Coord -> Integer
formula (x, y) = (x * x) + (3 * x) + (2 * x * y) + (y) + (y * y) + input

isValid :: Coord -> Bool
isValid c@(x, y) = x >= 0 && y >= 0 && (even . popCount . formula $ c)

manhattan :: Coord -> Coord -> Integer
manhattan (x1, y1) (x2, y2) = x' + y'
    where
        x' = if x1 > x2 then x1 - x2 else x2 - x1
        y' = if y1 > y2 then y1 - y2 else y2 - y1

main :: IO ()
main = do
    let start = (1, 1)
    let target = (31, 39)
    let entree (x, y) = filter isValid [(x, succ y), (x, pred y), (succ x, y), (pred x, y)]

    -- print $ bfs start target entree
    let heuristic c = manhattan c target
    print $ astar start target entree heuristic
