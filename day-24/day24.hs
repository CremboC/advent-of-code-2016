module Main where

import qualified Data.Map.Lazy as M
import qualified Data.Sequence as S
import Day24Search (astar)
import Data.List (permutations, find, (\\))
import Data.Maybe (fromJust)
import Debug.Trace

type Coord = (Int, Int)
type World = M.Map Coord Char

solve :: World -> Coord -> [Coord] -> Bool -> Int
solve world start targets shouldRet = minimum . map test $ perms 
    where
        perms = permutations targets
        heuristic to from = manhattan from to
        isValid loc = (world M.! loc) /= '#'
        nextEntries path = S.fromList . filter isValid $ [(x, succ y), (x, pred y), (succ x, y), (pred x, y)]
            where (_ S.:> (x, y)) = S.viewr path
        test perm = loop $ start : perm ++ (if shouldRet then [start] else [])
            where
                loop (from:to:xs) = steps + loop (to:xs)
                    where steps = pred . length $ astar from to nextEntries (heuristic to)
                loop _ = 0

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

main :: IO ()
main = do
    let f (y, row) = let f' (x, loc) = ((x, y), loc) in map f' row
    world <- M.fromList . concatMap f . zip [0..] . map (zip [0..]) . lines <$> readFile "test.txt"

    let isTarget (_, tile) = tile `notElem` ['.', '#']
    let targets = filter isTarget . M.toList $ world
    let start = fromJust . find ((== '0') . snd) $ targets

    -- print $ permutations $ (map fst $ targets \\ [start])


    print $ solve world (fst start) (map fst $ targets \\ [start]) False

    -- print $ solve world (fst start) (map fst $ targets \\ [start]) True