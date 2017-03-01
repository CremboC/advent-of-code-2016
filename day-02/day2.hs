import qualified Data.Map.Strict as Map
import Data.Tuple (swap)
import Debug.Trace

-- data Direction = U | D | L | R deriving (Show, Ord, Eq)
type Direction = Char
type Coordinate = (Int, Int)
type Loc = Int -> Coordinate
type Loc' = Coordinate -> Int

-- map for Part 1
locs' = Map.fromList [
        ((-1, 1), 1), ((0, 1), 2), ((1, 1), 3),
        ((-1, 0), 4), ((0, 0), 5), ((1, 0), 6),
        ((-1, -1), 7), ((0, -1), 8), ((1, -1), 9)
    ]

-- map for Part 2
locs2' = Map.fromList [
        ((0, 2), 1),
        ((-1, 1), 2), ((0, 1), 3), ((1, 1), 4),
        ((-2, 0), 5), ((-1, 0), 6), ((0, 0), 7), ((1, 0), 8), ((2, 0), 9),
        ((-1, -1), 0xA), ((0, -1), 0xB), ((1, -1), 0xC),
        ((0, -2), 0xD)
    ]

deltas = Map.fromList [('U', (0, 1)), ('D', (0, -1)), ('R', (1, 0)), ('L', (-1, 0))]

delta' :: Direction -> (Int, Int)
delta' dir = Map.findWithDefault (0, 0) dir deltas

(<+>) :: (Num a, Num t) => (a, t) -> (a, t) -> (a, t) 
(x, y) <+> (x', y') = (x + x', y + y')

move :: Loc -> Loc' -> Int -> Direction -> Int
move lc lc' start dir = if end == 0 then start else end
    where end = lc' (lc start <+> delta' dir)
            
step :: Int -> Loc -> Loc' -> [Direction] -> Int
step start lc lc' instr = foldl (\pos ins -> move lc lc' pos ins) start instr

solve :: [[Direction]] -> Loc -> Loc' -> [Int]
solve instrucs loc loc' = tail . reverse . foldl f [5] $ instrucs
    where f res ins = step (head res) loc loc' ins : res

flipMap :: Ord k => Map.Map a k -> Map.Map k a
flipMap mp = Map.fromList $ map swap (Map.assocs mp)

main :: IO ()
main = do
    instrucs <- lines <$> readFile "day2.txt"
    let find mp k = Map.findWithDefault (-999, -999) k mp   -- find for (Int -> Coordinate)
    let find' mp k = Map.findWithDefault 0 k mp             -- find for (Coordinate -> Int)
    print $ solve instrucs (find $ flipMap locs') (find' locs')
    print $ solve instrucs (find $ flipMap locs2') (find' locs2')