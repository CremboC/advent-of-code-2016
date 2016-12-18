module Main where

input :: String
input = ".^^^.^.^^^^^..^^^..^..^..^^..^.^.^.^^.^^....^.^...^.^^.^^.^^..^^..^.^..^^^.^^...^...^^....^^.^^^^^^^"
-- input = "..^^."
-- input = ".^^.^.^^^^"

size :: Int
size = length input

slice :: [a] -> Int -> Int -> [a]
slice xs i k = [x | (x,j) <- zip xs [1..k + 1], i < j]

patternToTile :: String -> Char
patternToTile "^^." = '^'
patternToTile ".^^" = '^'
patternToTile "^.." = '^'
patternToTile "..^" = '^'
patternToTile _ = '.'

nextRow :: String -> String
nextRow s = map f [0..size - 1]
    where f i = patternToTile $ slice temp i (i + 2)
          temp = ['.'] ++ s ++ ['.']

rows :: String -> [String]
rows s = s : (rows s')
    where s' = nextRow s

main = do
    -- part 1
    print $ length . filter (== '.') . concat . take 40 . rows $ input

    -- part 2
    print $ length . filter (== '.') . concat . take 400000 . rows $ input
