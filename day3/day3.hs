import Data.List.Split
import Data.List
import Data.Char
import qualified Data.Map as Map

-- convert "  142  23 543" -> [142, 23, 543]
-- wordsBy splits by some condition that defines what is not a word
-- map read converts the strings: ["142", "23", "543"] -> [142, 23, 543]
triangulate :: String -> [Int]
triangulate tri = map read $ wordsBy (not . isDigit) tri

-- converts [(0, 321), (1, 567), (0, 123), (1, 765)] -> [(0, [321, 123]), (1, [567, 765])]
-- equivalent to groupBy
sortAndGroup :: Ord k => [(k, a)] -> [(k, [a])]
sortAndGroup assocs = Map.assocs $ Map.fromListWith (++) [(k, [v]) | (k, v) <- assocs]

-- logic for the part 2 two: where there triangles below are [x01, x02, x03]
-- 101 301 501
-- 102 302 502
-- 103 303 503
-- 1. get a bunch of three lines [[101, 301, 501], [102, 302, 502], [103, 303, 503]]
-- 2. zip up to have index [[(0, 101), (1, 301), (2, 501)], ..]
-- 3. flatten into single list [(0, 101), (0, 102), ..]
-- 4. sort and group into [(0, [101, 102, 103]), ..]
-- 5. get only the snd element so we have triangles [[101, 102, 103], [301, 302, 303], ..]
triangulate' :: [[Int]] -> [[Int]]
triangulate' bunch = map snd $ sortAndGroup $ concat $ map (zip [0..]) bunch

-- checks there the given triangle [a, b, c] is a valid
-- valid triangle: sum of two edges is more than the third
valid :: [Int] -> Bool
valid [a, b, c] = a < b + c && b < a + c && c < b + a

main :: IO ()
main = do
    input <- readFile "input.txt"
    let lns = lines input
    -- part 1
    let triangles = map triangulate lns
    print $ length $ filter valid triangles
    -- part 2
    let bunchs = map (map triangulate) (chunksOf 3 lns)
    print $ length $ filter valid $ concat $ map triangulate' bunchs
