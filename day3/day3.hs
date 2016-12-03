import Data.List.Split
import Data.List
import Data.Char

-- convert "  142  23 543" -> [142, 23, 543]
-- words splits into words (delimited by whitespace)
-- map read converts the strings: ["142", "23", "543"] -> [142, 23, 543]
parse :: String -> [Int]
parse tri = map read $ words tri

-- checks there the given triangle [a, b, c] is a valid
-- valid triangle: sum of two edges is more than the third
triangle' :: [Int] -> Bool
triangle' [a, b, c] = a < b + c && b < a + c && c < b + a

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = map parse (lines input)

    -- part 1
    print $ length $ filter triangle' parsed

    -- part 2
    -- 1. split the original parsed input into groups of 3 [[[x, y, z], [a, b, c], [f, g, h]], ..]
    -- 2. take each bunch and transpose it [[x, y, z], [a, b, c], [f, g, h]] --> [[x, a, f], [y, b, g], [z, c, h]]
    -- 3. concat == flatten all the bunches
    -- 4-5. get length of valid triangles
    print $ length $ filter triangle' $ concat $ map transpose $ chunksOf 3 parsed
