import Data.List
import Data.Ord (comparing)

grp :: String -> [(Char, Int)]
grp str = map (\l -> (head l, length l)) . group . sort $ str

main :: IO ()
main = do
    input <- map grp . transpose . lines <$> readFile "input.txt"
    -- part 1
    print $ map fst . map (maximumBy (comparing snd)) $ input
    -- part 2
    print $ map fst . map (minimumBy (comparing snd)) $ input