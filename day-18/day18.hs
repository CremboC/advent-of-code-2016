module Main where
    
input :: [Bool]
input = map f ".^^^.^.^^^^^..^^^..^..^..^^..^.^.^.^^.^^....^.^...^.^^.^^.^^..^^..^.^..^^^.^^...^...^^....^^.^^^^^^^"
    where f '.' = False
          f _ = True

nextRow :: [Bool] -> [Bool]
nextRow (x:y:z:xs) = (x /= z) : nextRow (y:z:xs)
nextRow _ = []

rows :: [Bool] -> [[Bool]]
rows s = s : (rows s')
    where s' = nextRow ([False] ++ s ++ [False])

main = do
    -- part 1
    print $ length . filter (== False) . concat . take 40 . rows $ input

    -- part 2
    print $ length . filter (== False) . concat . take 400000 . rows $ input
