module Main where

input :: String
input = ".^^^.^.^^^^^..^^^..^..^..^^..^.^.^.^^.^^....^.^...^.^^.^^.^^..^^..^.^..^^^.^^...^...^^....^^.^^^^^^^"
-- input = "..^^."
-- input = ".^^.^.^^^^"

size :: Int
size = length input

patternToTile :: String -> Char
patternToTile "^^." = '^'
patternToTile ".^^" = '^'
patternToTile "^.." = '^'
patternToTile "..^" = '^'
patternToTile _ = '.'

nextRow :: String -> String
nextRow (a:b:c:xs) = patternToTile (a:b:c:[]) : nextRow (b:c:xs)
nextRow _ = []

rows :: String -> [String]
rows s = s : (rows s')
    where s' = nextRow (['.'] ++ s ++ ['.'])

main = do
    -- part 1
    print $ length . filter (== '.') . concat . take 40 . rows $ input

    -- part 2
    print $ length . filter (== '.') . concat . take 400000 . rows $ input
