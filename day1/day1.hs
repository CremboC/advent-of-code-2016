-- {-# LANGUAGE OverloadedStrings #-}

import Data.List.Split
import Text.Regex.Posix

data Facing = N | S | E | W deriving Eq
data Direction = R | L deriving Eq
type Steps = Int
type Move = (Direction, Steps)
type Coordinate = (Int, Int)

-- parses a single move from the input file
parseMove :: String -> Move
parseMove mv = (direction, read steps :: Int)
    where
        direction = if dir == "R" then R else L
        (_, _, _, [dir, steps]) = mv =~ "(R|L)([0-9]+)" :: (String, String, String, [String])

-- Calculate next location depending on given location and facing
nextLocation :: Coordinate -> Facing -> Coordinate
nextLocation location direction =  let (x, y) = location in case direction of
    N -> (x, succ y)
    S -> (x, pred y)
    E -> (succ x, y)
    W -> (pred x, y)

-- Steps the required amount from the given start and facing. Returns the full path.
step :: Int -> [Coordinate] -> Facing -> [Coordinate]
-- step steps path direction = let runner = \steps path ->
step steps path direction = let loc = nextLocation (last path) direction in
    if steps == 0 then tail path else step (pred steps) (path ++ [loc]) direction

-- Do a single move instruction (e.g. R4)
doMove :: Move -> Facing -> Coordinate -> (Facing, [Coordinate])
doMove move direction location = (newDirection, step steps [location] newDirection)
    where
        (nxtDir, steps) = move
        newDirection = case direction of
            N -> if nxtDir == R then E else W
            S -> if nxtDir == R then W else E
            E -> if nxtDir == R then S else N
            W -> if nxtDir == R then N else S

createPath :: [Move] -> Facing -> [Coordinate] -> [Coordinate]
createPath moves direction path
    | length moves == 0 = path
    | otherwise = createPath ms newDir (path ++ newLoc)
    where
        (newDir, newLoc) = doMove m direction (last path)
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
