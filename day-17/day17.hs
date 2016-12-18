{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-local-binds -fno-warn-unused-pattern-binds -fno-warn-unused-top-binds -fno-warn-unused-matches #-}

module Main where

import qualified Data.ByteString.Char8 as BS 
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as BS16
import Day17Search (bfs, bfsMax)
import Debug.Trace
import Data.Maybe (catMaybes)

type Direction = Char
type Coord = (Integer, Integer)
type Node = (Coord, [Direction])

md5 :: String -> String
md5 = BS.unpack . BS16.encode . MD5.hash . BS.pack

directionTo :: Coord -> Coord -> Direction
directionTo (x1, y1) (x2, y2) 
    | x1 == x2 = if y1 > y2 then 'U' else 'D'
    | y1 == y2 = if x1 > x2 then 'L' else 'R'

validLocs :: [Coord]
validLocs = [(a, b) | a <- [1..4], b <- [1..4]]

nextEntries :: String -> Node -> [Node]
nextEntries input (loc, path) = map n available
    where
        (x, y) = loc
        available = filter (`elem` validLocs) . catMaybes $ [
                if up then Just (x, pred y) else Nothing,
                if down then Just (x, succ y) else Nothing,
                if left then Just (pred x, y) else Nothing,
                if right then Just (succ x, y) else Nothing
            ]
        [up, down, left, right] = map isOpen . take 4 . md5 $ input ++ path
        isOpen a | a `elem` ['b', 'c', 'd', 'e', 'f'] = True
        isOpen _ = False
        n coord = (coord, path ++ [loc `directionTo` coord])

main :: IO ()
main = do
    let start = ((1, 1), "") :: Node
    let target (loc, _) = loc == (4, 4)

    -- part 1
    print $ bfs start target (nextEntries "edjrjqaa")

    -- part 2
    print $ maximum . map snd . take 10000 $ bfsMax start target (nextEntries "edjrjqaa")
