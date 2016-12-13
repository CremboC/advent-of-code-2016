{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Day13Astar
import Numeric
import Data.Char
import qualified Data.Text as T
import Debug.Trace
import Data.Bits (popCount)

input :: Integer
input = 1352

formula :: Coord -> Integer
formula (x, y) = (x * x) + (3 * x) + (2 * x * y) + (y) + (y * y) + input

isValid :: Coord -> Bool
isValid c@(x, y) = x >= 0 && y >= 0 && (even . popCount . formula $ c)

main :: IO ()
main = do
    let start = (1, 1)
    let target = (31, 39)
    print $ astar start target isValid
