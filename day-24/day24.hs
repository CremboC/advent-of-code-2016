module Main where

import qualified Data.Map.Lazy as M
import qualified Data.Sequence as S
import Day24Search (astar)
import Data.List (permutations, find, (\\))
import Data.Maybe (fromJust)

type Coord = (Int, Int)
type World = M.Map Coord Char
type DistanceMap = M.Map (Coord, Coord) Int

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = x' + y'
    where
        x' = if x1 > x2 then x1 - x2 else x2 - x1
        y' = if y1 > y2 then y1 - y2 else y2 - y1

combinations :: Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations 0 _  = [[]]
combinations k (x:xs) = x_start ++ others
    where x_start = [ x : rest | rest <- combinations (k-1) xs ]
          others  = if k <= length xs then combinations k xs else []

solve :: World -> Coord -> Coord -> Int
solve world start end = length $ astar start end nextEntries heuristic
    where
        heuristic from = manhattan from end
        isValid loc = (world M.! loc) /= '#'
        nextEntries path = S.fromList . filter isValid $ [(x, succ y), (x, pred y), (succ x, y), (pred x, y)]
            where (_ S.:> (x, y)) = S.viewr path

shortest :: DistanceMap -> [Coord] -> Int
shortest distances path = loop path
    where 
        loop (from:to:xs) = (distances M.! (from, to)) + loop (to:xs)
        loop _ = 0

main :: IO ()
main = do
    let f (y, row) = let f' (x, loc) = ((x, y), loc) in map f' row
    world <- M.fromList . concatMap f . zip [0..] . map (zip [0..]) . lines <$> readFile "input.txt"

    let isTarget (_, tile) = tile `notElem` ['.', '#']
    let targets = filter isTarget . M.toList $ world
    let start = fromJust . find ((== '0') . snd) $ targets

    let paths = combinations 2 (map fst $ targets)
    let f' path@[from, to] = let steps = pred $ solve world from to in [((from, to), steps), ((to, from), steps)]
    let distanceMap = M.fromList . concatMap f' $ paths

    let startLoc = fst start

    -- part 1
    print $ minimum . map (shortest distanceMap . (startLoc : )) . permutations $ (map fst $ targets \\ [start])

    -- part 2
    print $ minimum . map (shortest distanceMap . (startLoc : ) . (++ [startLoc])) . permutations $ (map fst $ targets \\ [start])