import Data.List
import Data.List.Split
import Text.Regex.Posix
import Debug.Trace

data Instruction = Rect Int Int | RotateCol Int Int | RotateRow Int Int deriving (Show, Eq)

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start + 1) . drop start

parse :: String -> Instruction
parse str 
    | "rect" `isPrefixOf` str = 
        let (_, _, _, [x, y]) = str =~ "rect ([0-9]+)x([0-9]+)" :: (String, String, String, [String]) in 
            Rect (read x) (read y)
    | "column" `isInfixOf` str = 
        let (_, _, _, [x, rot]) = str =~ "column x=([0-9]+) by ([0-9]+)" :: (String, String, String, [String]) in 
            RotateCol (read x) (read rot)
    | "row" `isInfixOf` str = 
        let (_, _, _, [y, rot]) = str =~ "row y=([0-9]+) by ([0-9]+)" :: (String, String, String, [String]) in 
            RotateRow (read y) (read rot)

mkRect :: Int -> Int -> [[Char]] -> [[Char]]
mkRect x y state = state' ++ rest
    where 
        state' = map turnOn . map (splitAt x) $ change
        turnOn (_, r) = replicate x '#' ++ r
        (change, rest) = splitAt y state

rotateCol :: Int -> Int -> [[Char]] -> [[Char]]
rotateCol x rot state = transpose state''
    where 
        state'' = (slice 0 (pred x) transposed) ++ [state'] ++ (slice (succ x) (length transposed) transposed)
        state' = slice (len - rot) (len * 2 - rot - 1) (cycle col)
        len = length col
        col = transposed !! x
        transposed = transpose state

rotateRow :: Int -> Int -> [[Char]] -> [[Char]]
rotateRow y rot state = (slice 0 (pred y) state) ++ [state'] ++ (slice (succ y) (length state) state)
    where 
        state' = slice (len - rot) (len * 2 - rot - 1) (cycle row)
        len = length row
        row = state !! y

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


