{-# LANGUAGE ViewPatterns #-}

import Data.List.Extra
import qualified Data.Map.Strict as M
import Data.Char
import Debug.Trace

type MinMax = [Int] -> Int
type Bot = Int
type Bin = Int
type State = M.Map Bot [Int]
type Modifier = State -> (State, Bool)
data Giving = GBot Bot MinMax | GBin Bin MinMax

isAlphaSpace :: Char -> Bool
isAlphaSpace x = isAlpha x || isSpace x

isGoes :: String -> Bool
isGoes str = "value" `isPrefixOf` str

parse :: String -> Modifier
parse str | isGoes str = let [chip, bot] = parsed in goes bot chip
    where parsed = map (read :: String -> Int) . wordsBy isAlphaSpace $ str
parse str = gives (read bot) (determine a aN minimum) (determine b bN maximum)
    where 
        determine s n f = if s == "bot" 
            then GBot (read n) f
            else GBin (read n) f
        [_, bot, _, _, _, a, aN, _, _, _, b, bN] = words str

goes :: Bot -> Int -> State -> (State, Bool)
goes bot chip state = (giveChip bot chip state, True)

-- bot give give state -> state'
gives :: Bot -> Giving -> Giving -> State -> (State, Bool)
gives bot g1 g2 state | canGive state = (state', True)
    where
        state' = f' g2 . f' g1 $ state
        canGive s = checkBy ((== 2) . length) bot s
        f' :: Giving -> State -> State
        f' (GBot to minmax) s = transferChip bot to (minmax $ s M.! bot) s
        f' (GBin to minmax) s = let chip = minmax $ s M.! bot in 
            traceShow ("Bin " ++ (show to) ++ " " ++ (show chip)) $ removeChip bot chip s

gives _ _ _ state = (state, False)

transferChip :: Bot -> Bot -> Int -> State -> State
transferChip from to chip state = removeChip from chip $ giveChip to chip state

removeChip :: Bot -> Int -> State -> State
removeChip from chip state = M.alter f from state
    where
        f Nothing = Nothing
        f (Just chips) = Just (chips \\ [chip])

giveChip :: Bot -> Int -> State -> State
giveChip to chip state = M.alter f to state
    where
        f Nothing    = Just [chip]
        f (Just old) = Just (old ++ [chip])

checkBy :: (Ord k, Foldable f) => (f a -> Bool) -> k -> M.Map k (f a) -> Bool
checkBy f k m = case M.lookup k m of
    (Just is) -> f is
    Nothing   -> False

isTarget (bot, chips) = null $ [17, 61] \\ (sort chips)
target = filter isTarget . filter ((== 2) . length . snd) . M.toList

exec :: [Modifier] -> Bool -> State -> State
exec [] _ state = state
exec (m:ms) stop state = if foundTarget then state' else exec ms' stop state'
    where
        foundTarget = stop && (not . null $ target state)
        ms' = if modified then ms else ms ++ [m]
        (state', modified) = m state

main :: IO ()
main = do
    instructions <- fmap parse . lines <$> readFile "input.txt"
    -- part 1
    print $ target $ exec instructions True M.empty
    -- part 2
    print $ "Check stdout for part 2. " ++ (show $ not . null $ exec instructions False M.empty)