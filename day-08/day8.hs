import Data.List
import Data.List.Split

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

parse :: String -> [String] -> [String]
parse str 
    | "rect" `isPrefixOf` str = let [x, y] = splitOn "x" . last . words $ str in mkRect (read x) (read y)
    | "column" `isInfixOf` str = let (x, rot) = parseRotate str in transpose . rotate x rot . transpose
    | "row" `isInfixOf` str = let (y, rot) = parseRotate str in rotate y rot
    | otherwise = (\s -> s)

mkRect :: Int -> Int -> [String] -> [String]
mkRect x y state = state' ++ rest
    where 
        state' = map turnOn . map (splitAt x) $ change
        turnOn (_, r) = replicate x '#' ++ r
        (change, rest) = splitAt y state

rotate :: Int -> Int -> [String] -> [String]
rotate n rot state = start ++ [subject'] ++ end
    where 
        (start, subject, end) = splitAt' n state
        subject' = slice (len - rot) (len * 2 - rot - 1) (cycle . head $ subject)
        len = length . head $ subject
        
main :: IO ()
main = do
    instructions <- fmap parse . lines <$> readFile "input.txt"
    let line = replicate 50 '.'
    let initialState = replicate 6 line
    -- part 1
    print $ length . filter (=='#') . concat . foldl (\state f -> f state) initialState $ instructions
    -- part 2
    print $ foldl (\state f -> f state) initialState $ instructions