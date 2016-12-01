-- {-# LANGUAGE OverloadedStrings #-}

import Data.List.Split
import Text.Regex.Posix

data Facing = N | S | E | W deriving (Read, Show, Enum, Eq, Ord)
data Direction = R | L deriving (Read, Show, Enum, Eq, Ord)
type Steps = Int
type Move = (Direction, Steps)
type Coordinate = (Int, Int)

move :: Move -> Facing -> Coordinate -> (Facing, Coordinate)
move mv facing xy
    | facing == N = case nxtDir of
        R -> (E, (x + steps, y))
        L -> (W, (x - steps, y))
    | facing == S = case nxtDir of
        R -> (W, (x - steps, y))
        L -> (E, (x + steps, y))
    | facing == E = case nxtDir of
        R -> (S, (x, y - steps))
        L -> (N, (x, y + steps))
    | facing == W = case nxtDir of
        R -> (N, (x, y + steps))
        L -> (S, (x, y - steps))
    where
        (x, y) = xy
        (nxtDir, steps) = mv

createMove :: String -> Move
createMove mv = (direction, read steps :: Int)
    where
        direction = case dir of
            "R" -> R
            "L" -> L
        (_, _, _, [dir, steps]) = mv =~ "(R|L)([0-9]+)" :: (String, String, String, [String])

doMoves :: [Move] -> Facing -> Coordinate -> Coordinate
doMoves moves facing loc
    | length moves == 0 = loc
    | otherwise = doMoves ms newFacing newLoc
    where
        (newFacing, newLoc) = move m facing loc
        (m : ms) = moves

manhattanDist p1 p2 =
  sum $ zipWith (\x y -> abs (x - y)) p1 p2

day1p1 = do
    input <- readFile "day1.txt"
    return $ let moves = splitOn ", " input in
        doMoves (map createMove moves) N (0, 0)

main = do
    val <- day1p1
    let (x, y) = val in print $ manhattanDist [0, 0] [x, y]
