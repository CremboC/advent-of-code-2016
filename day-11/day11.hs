{-# LANGUAGE ViewPatterns, OverloadedStrings, TupleSections #-}

-- import Data.List.Extra
import Day11Extra (listDifference, ordNub)
import qualified Data.Text as T
import Debug.Trace
import Data.Maybe
import qualified Data.Map.Strict as M
-- import qualified Data.Sequence as Seq
-- import Data.Sequence (Seq, viewl, (><), ViewL((:<), EmptyL))
-- import Data.Hashable
import Data.Ord (comparing)
import Data.List (maximumBy, sort, permutations, tails)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, viewl, (><), ViewL((:<), EmptyL))
-- import Data.List.Extra (unstableSort)

type Lift = Int
type Floor = Int
type Steps = Int
type Load = [T.Text]
type S' = [[T.Text]]
type State = (S', Floor, Steps)

input :: S'
input = [
        sort ["TG", "TM", "PG", "SG"],   -- floor 1
        sort ["PM", "SM"],               -- floor 2 
        sort ["PrG", "PrM", "RG", "RM"], -- floor 3
        []                          -- floor 4
    ]

completed :: [[a]] -> Bool
completed = all null . init

compatible :: T.Text -> T.Text -> Bool
compatible a b = (T.init a) == (T.init b) && (T.last a) /= (T.last b)

isLegal :: [T.Text] -> Bool
isLegal [] = True
isLegal [_] = True
isLegal lst = if null generators then True else all f chips
    where 
        f chip = any (compatible chip) lst
        generators = filter ((== 'G') . T.last) lst
        chips = filter ((== 'M') . T.last) lst

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [ y:ys | y:xs' <- tails xs
                            , ys <- combinations (n-1) xs']

-- combinations :: Show a => Int -> Int -> [a] -> [[a]]
-- combinations n m lst = concatMap f' [n..m]
--     where f' i = map (take i) (permutations lst)

lcombinations :: Int -> [T.Text] -> [Load]
lcombinations n lst = filter isLegal . combinations n $ lst

move :: S' -> Floor -> Floor -> Load -> Maybe S'
move state from to load = if legal 
    then Just final
    else Nothing
    where 
        legal = and [isLegal to', isLegal from']

        final = M.elems floorMap'
        floorMap' = M.adjust (\_ -> to') to $ M.adjust (\_ -> from') from floorMap

        from' = listDifference (state !! from) load
        to' = (state !! to) ++ load
    
        floorMap = M.fromList $ zip [0..] state

data Direction = Up | Down deriving Show

nextStates :: State -> Direction -> [State]
nextStates (state, lift, moves) Up | succ lift <= 3 = states
    where
        states = 
            ordNub $
            fmap ((, succ lift, succ moves)) $ 
            fmap (\s -> sort s)
            catMaybes $ 
            fmap (move state lift (succ lift)) $
            filter (isLegal . (flr `listDifference`)) $ lloads
        lloads = (lcombinations 1 flr) ++ (lcombinations 2 flr)
        flr = state !! lift
nextStates (state, lift, moves) Down | pred lift >= 0 =
    if length (state !! (pred lift)) == 0 
        then []
        else states
    where
        states = 
            ordNub $ 
            fmap ((, pred lift, succ moves)) $ 
            fmap (\s -> sort s)
            catMaybes $ 
            fmap (move state lift (pred lift)) $
            filter (isLegal . (flr `listDifference`)) $ lloads
        lloads = (lcombinations 1 flr)
        flr = state !! lift
nextStates _ _ = []

solve :: Seq State -> Steps
solve (viewl -> EmptyL) = error "Shouldn't happen"
solve (viewl -> (s :< xs)) | completed state = moves where (state, _, moves) = s
solve (viewl -> (s :< xs)) = traceShow (length xs, moves) $ solve pstates
    where 
        -- bestState = fst . maximumBy (comparing snd) . map (\s@(state, _, _) -> (s, nh state)) $ pstates
        pstates = xs >< (Seq.fromList $ ordNub $ states')
        states' = (nextStates s Up) ++ (nextStates s Down)
        (state, lift, moves) = s

-- normalize n = (fromIntegral (length n) / 10)

-- nh [a, b, c, d] = (length d) - (length c) - (length b) - (length a)

-- heuristic
-- heuristic :: [[a]] -> 
-- heuristic [[], [], [], d] = 10
-- heuristic [[], [], c, d] = 2
-- heuristic [[], b, c, d] = 1
-- heuristic _ = 0

-- prune :: Seq State -> [State]
-- prune states = map fst . filter ((== maxH) . snd) $ hs
--     where 
--         maxH = snd . maximumBy (comparing snd) $ hs
--         hs = map (\s@(state, _, _) -> (s, heuristic state)) states

end = [
        [],   -- floor 1
        [],               -- floor 2 
        [], -- floor 3
        sort ["TG", "TM", "PG", "SG", "PM", "SM", "PrG", "PrM", "RG", "RM"] -- floor 4
    ]

main = do
    let start = (input, 0, 0)
    -- print $ solve [(end, 3, 35)]

    -- print $ nub . map sort . combinations' 2 $ ["TG", "TM", "PG", "SG"]
    -- print $ filter isLegal . combinations' 2 $ ["TG", "TM", "PG", "SG"]

    print $ solve (Seq.fromList [start])
    -- let after1 = [] ++ (filter (not . (`elem` [])) $ (nextStates start Up) ++ (nextStates start Down))
    -- print $ after1
    -- let start' = head after1
    -- let (state', lift', moves') = start'
    -- let after2 = (nextStates start' Up) ++ (nextStates start' Down)
    -- print $ after2

    -- let after3 = concatMap (\s -> (nextStates s Up) ++ (nextStates s Down)) after2
    -- let after3' = map (\s@(state, _, _) -> (s, heuristic state)) after3
    -- let maxHeuristic3 = snd . maximumBy (comparing snd) $ after3'
    -- print $ map fst . filter ((== maxHeuristic3) . snd) $ after3'
    -- print $ map (\s@(state, _, _) -> (s, heuristic state)) after3
    -- print $ sortBy heuristic [ [[1], [2], [3], []], [[], [1], [1], [2]]]
    -- print $ filter (\() -> )after3