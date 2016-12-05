import Data.List
import Data.Ord
import Data.Map (Map)
import qualified Data.Map.Strict as Map

grp :: String -> [(Char, Int)]
grp str = map (\l -> (head l, length l)) . group . sort $ str

main :: IO ()
main = do
    input <- transpose . lines <$> readFile "input.txt"
    print $ map fst $ map (maximumBy (comparing snd)) $ map grp input