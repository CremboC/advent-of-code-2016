-- {-# LANGUAGE OverloadedStrings #-}

import Data.List.Split
import Text.Regex.Posix

data Facing = N | S | E | W deriving Eq
data Direction = R | L deriving Eq
type Steps = Int
type Move = (Direction, Steps)
type Coordinate = (Int, Int)

parseMove :: String -> Move
parseMove mv = (direction, read steps :: Int)
    where
        direction = if dir == "R" then R else L
        (_, _, _, [dir, steps]) = mv =~ "(R|L)([0-9]+)" :: (String, String, String, [String])

nextLocation :: Coordinate -> Facing -> Coordinate
nextLocation location direction =  case direction of
    N -> (x, succ y)
    S -> (x, pred y)
    E -> (succ x, y)
    W -> (pred x, y)
    where
        (x, y) = location

step :: Int -> [Coordinate] -> Facing -> [Coordinate]
step steps path direction =
    if steps == 0
        then tail path
        else step (pred steps) (path ++ [loc]) direction
    where
        loc = nextLocation (last path) direction

doMove :: Move -> Facing -> Coordinate -> (Facing, [Coordinate])
doMove move facing location = (newDirection, step steps [location] newDirection)
    where
        (nxtDir, steps) = move
        newDirection = case facing of
            N -> if nxtDir == R then E else W
            S -> if nxtDir == R then W else E
            E -> if nxtDir == R then S else N
            W -> if nxtDir == R then N else S

createPath :: [Move] -> Facing -> [Coordinate] -> [Coordinate]
createPath moves facing path
    | length moves == 0 = path
    | otherwise = createPath ms newFacing (path ++ newLoc)
    where
        (newFacing, newLoc) = doMove m facing (last path)
        (m : ms) = moves

day1p1 = do
    input <- readFile "day1.txt"
    return $ let moves = map parseMove (splitOn ", " input) in
        last $ createPath moves N [(0, 0)]

findFirstDuplicate :: [Coordinate] -> Coordinate
findFirstDuplicate coords = let c : cs = coords in
    if c `elem` cs then c else findFirstDuplicate cs

day1p2 = do
    input <- readFile "day1.txt"
    return $ let moves = map parseMove (splitOn ", " input) in
        findFirstDuplicate $ createPath moves N [(0, 0)]

main = do
    val <- day1p1
    let (x, y) = val in print $ abs x + abs y
    val2 <- day1p2
    let (x, y) = val2 in print $ abs x + abs y
