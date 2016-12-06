import Data.List
import Data.Ord (comparing)

main :: IO ()
main = do
    input <- map (group . sort) . transpose . lines <$> readFile "input.txt"
    -- part 1
    -- print input
    print $ map (head . maximumBy (comparing length)) $ input
    -- print $ map fst . map (maximumBy (comparing snd)) $ input
    -- part 2
    print $ map (head . minimumBy (comparing length)) $ input
    -- print $ map fst . map (minimumBy (comparing snd)) $ input