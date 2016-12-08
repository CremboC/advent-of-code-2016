import Data.List
import Data.List.Split
import Debug.Trace

data Instruction = Rect Int Int | RotateCol Int Int | RotateRow Int Int deriving (Show, Eq)

slice :: Int -> Int -> [a] -> [a]
slice start end xs = take (end - start + 1) . drop start $ xs

slice' :: Int -> Int -> [a] -> ([a], [a], [a])
slice' start end xs = (pre, slice start end xs, post)
    where 
        (_, post) = splitAt (succ end) xs
        (pre, _) = splitAt start xs

splitAt' :: Int -> [a] -> ([a], [a], [a])
splitAt' i xs = slice' i i xs

parseRotate :: String -> (Int, Int)
parseRotate str = (read n, read rot)
    where 
        [n', _, rot] = drop 2 . words $ str
        n = last . splitOn "=" $ n' 

parse :: String -> Instruction
parse str 
    | "rect" `isPrefixOf` str = let [x, y] = splitOn "x" . last . words $ str in Rect (read x) (read y)
    | "column" `isInfixOf` str = let (x, rot) = parseRotate str in RotateCol x rot
    | "row" `isInfixOf` str = let (y, rot) = parseRotate str in RotateRow y rot

mkRect :: Int -> Int -> [[Char]] -> [[Char]]
mkRect x y state = state' ++ rest
    where 
        state' = map turnOn . map (splitAt x) $ change
        turnOn (_, r) = replicate x '#' ++ r
        (change, rest) = splitAt y state

rotateCol :: Int -> Int -> [[Char]] -> [[Char]]
rotateCol x rot state = transpose $ start ++ [col'] ++ end
    where 
        col' = slice (len - rot) (len * 2 - rot - 1) (cycle . head $ col)
        len = length . head $ col
        (start, col, end) = splitAt' x transposed
        transposed = transpose state

rotateRow :: Int -> Int -> [[Char]] -> [[Char]]
rotateRow y rot state = start ++ [row'] ++ end
    where 
        (start, row, end) = splitAt' y state
        row' = slice (len - rot) (len * 2 - rot - 1) (cycle . head $ row)
        len = length . head $ row

create :: [Instruction] -> [[[Char]] -> [[Char]]]
create [] = [(\s -> s)] -- noop
create (inst:xs) = (match inst) : (create xs)
    where 
        match (Rect x y) = mkRect x y
        match (RotateCol x rot) = rotateCol x rot
        match (RotateRow y rot) = rotateRow y rot

main :: IO ()
main = do
    instructions <- fmap parse . lines <$> readFile "input.txt"
    let f' = create instructions
    let line = replicate 50 '.'
    let initialState = replicate 6 line
    -- part 1
    print $ length . filter (=='#') . concat . foldl (\state f -> f state) initialState $ f'
    -- part 2
    print $ foldl (\state f -> f state) initialState $ f'