{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List.Split
import qualified Data.Map.Strict as Map

data Direction = U | D | L | R deriving Show
type Instruction = [Direction]
type Coordinate = (Int, Int)
type Loc = Int -> Coordinate
type Loc' = Coordinate -> Int

dir' :: Char -> Direction
dir' 'U' = U
dir' 'D' = D
dir' 'L' = L
dir' 'R' = R

-- map for Part 1
locs' = Map.fromList [
        ((-1, 1), 1),
        ((0, 1), 2),
        ((1, 1), 3),
        ((-1, 0), 4),
        ((0, 0), 5),
        ((1, 0), 6),
        ((-1, -1), 7),
        ((0, -1), 8),
        ((1, -1), 9)
    ]

-- (a, b) -> (b, a)
locs = Map.fromList $ map (\x -> (snd x, fst x)) (Map.assocs locs')

-- map for Part 2
locs2' = Map.fromList [
        ((0, 2), 1),
        ((-1, 1), 2),
        ((0, 1), 3),
        ((1, 1), 4),
        ((-2, 0), 5),
        ((-1, 0), 6),
        ((0, 0), 7),
        ((1, 0), 8),
        ((2, 0), 9),
        ((-1, -1), 0xA),
        ((0, -1), 0xB),
        ((1, -1), 0xC),
        ((0, -2), 0xD)
    ]

-- (a, b) -> (b, a)
locs2 = Map.fromList $ map (\x -> (snd x, fst x)) (Map.assocs locs2')



move :: Int -> Direction -> Loc -> Loc' -> Int
move start dir lc lc' = if end == 0 then start else end
    where
        end = case dir of
            U -> lc' (x, succ y)
            D -> lc' (x, pred y)
            R -> lc' (succ x, y)
            L -> lc' (pred x, y)
        (x, y) = lc start


step :: Instruction -> Int -> Loc -> Loc' -> Int
step instr start lc lc' = foldl (\pos ins -> move pos ins lc lc') start instr

compute loc loc' = do
    input <- readFile "day2.txt"
    let ins = lines input
    return $ let instr = map (\i -> map dir' i) ins in
        map (\i -> step i 5 loc loc') instr

locp1 :: Loc
locp1 n = Map.findWithDefault (-999, -999) n locs

locp1' :: Loc'
locp1' n = Map.findWithDefault 0 n locs'

locp2 :: Loc
locp2 n = Map.findWithDefault (-999, -999) n locs2

locp2' :: Loc'
locp2' n = Map.findWithDefault 0 n locs2'

main = do
    val <- compute locp1 locp1'
    print $ val
    val2 <- compute locp2 locp2'
    print $ val2

-- part 1
-- 1 2 3
-- 4 5 6
-- 7 8 9

-- part 2
--     1
--   2 3 4
-- 5 6 7 8 9
--   A B C
--     D