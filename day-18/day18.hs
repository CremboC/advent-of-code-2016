module Main where

input :: String
input = ".^^^.^.^^^^^..^^^..^..^..^^..^.^.^.^^.^^....^.^...^.^^.^^.^^..^^..^.^..^^^.^^...^...^^....^^.^^^^^^^"
-- input = "..^^."
-- input = ".^^.^.^^^^"

size :: Int
size = length input

matchesAt :: String -> Int -> Char
matchesAt xs i = f (drop i xs)
    where 
        f ('^':'^':'.':xs) = '^'
        f ('.':'^':'^':xs) = '^'
        f ('^':'.':'.':xs) = '^'
        f ('.':'.':'^':xs) = '^'
        f _ = '.'

nextRow :: String -> String
nextRow s = map f [0..size - 1]
    where f i = matchesAt temp i
          temp = ['.'] ++ s ++ ['.']

rows :: String -> [String]
rows s = s : (rows s')
    where s' = nextRow s

main = do
    -- part 1
    print $ length . filter (== '.') . concat . take 40 . rows $ input

    -- part 2
    print $ length . filter (== '.') . concat . take 400000 . rows $ input
