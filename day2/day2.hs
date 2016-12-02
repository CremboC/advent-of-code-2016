import qualified Data.Map.Strict as Map

data Direction = U | D | L | R deriving Show
type Instruction = [Direction]
type Coordinate = (Int, Int)
type Loc = Int -> Coordinate
type Loc' = Coordinate -> Int

flipMap mp = Map.fromList $ map (\x -> (snd x, fst x)) (Map.assocs mp)

dir' :: Char -> Direction
dir' 'U' = U
dir' 'D' = D
dir' 'L' = L
dir' 'R' = R

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

move :: Loc -> Loc'  -> Int -> Direction -> Int
move lc lc' start dir  = if end == 0 then start else end
    where
        end = case dir of
            U -> lc' (x, succ y)
            D -> lc' (x, pred y)
            R -> lc' (succ x, y)
            L -> lc' (pred x, y)
        (x, y) = lc start

step :: Int -> Loc -> Loc' -> Instruction -> Int
step start lc lc' instr = foldl (\pos ins -> move lc lc' pos ins) start instr

compute :: Loc -> Loc' -> IO [Int]
compute loc loc' = do
    input <- readFile "day2.txt"
    let ins = lines input
    return $ let instr = map (\i -> map dir' i) ins in
        map (\i -> step 5 loc loc' i) instr

main :: IO ()
main = do
    val <- let
            loc = \n -> Map.findWithDefault (-999, -999) n (flipMap locs')
            loc' = \n -> Map.findWithDefault 0 n locs'
        in compute loc loc'
    print $ val

    val2 <- let
            loc = \n -> Map.findWithDefault (-999, -999) n (flipMap locs2')
            loc' = \n -> Map.findWithDefault 0 n locs2'
        in compute loc loc'
    print $ val2