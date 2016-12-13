{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Day13Astar
import Numeric
import Data.Char
import qualified Data.Text as T
import Debug.Trace

input :: Integer
input = 1352

formula :: Coord -> Integer
formula (x, y) = (x * x) + (3 * x) + (2 * x * y) + (y) + (y * y) + input

bin :: Integer -> T.Text
bin n = T.pack $ showIntAtBase 2 intToDigit n ""

ones :: T.Text -> Integer
ones s = toInteger . T.length . T.filter (== '1') $ s

nonWall :: Coord -> Bool
nonWall c = (== 0) . (`rem` 2) . ones . bin . formula $ c

isWall :: Coord -> Bool
isWall c = (/= 0) . (`rem` 2) . ones . bin . formula $ c

main :: IO ()
main = do
    let start = (1, 1)
    let target = (31, 39)
    print $ astar start target nonWall
