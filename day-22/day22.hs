{-# LANGUAGE TupleSections, TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Day22Parse
import Day22Search (bfs)
import Data.Maybe (catMaybes)
import Data.List (foldl')
import Debug.Trace
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data State = State { snodes :: (M.Map Coord Node), sgoal :: Coord } deriving (Show, Eq, Ord)
-- newtype NPair = NPair (Coord, Coord) deriving (Ord, Show)
newtype Pair = Pair (Coord, Coord) deriving (Ord, Show)
instance Eq Pair where
    (Pair (a1, b1)) == (Pair (a2, b2)) = (a1 == b1 && a2 == b2) || (a1 == b2 && a2 == b1)

asMap :: M.Map Int [Node] -> Node -> M.Map Int [Node]
asMap m n = M.alter f (available n) m
    where f (Just s) = Just (n : s)
          f Nothing  = Just [n]

pairs :: M.Map Int [Node] -> Node -> [(Coord, Coord)]
pairs m n = map npair . concat . M.elems . M.dropWhileAntitone f $ m
    where f k = (used n) > k
          npair n' = (loc n, loc n')

viablePairs :: [Node] -> [(Coord, Coord)]
viablePairs nodes = concatMap (pairs byAvailable) . filter ((/= 0) . used) $ nodes
    where byAvailable = foldl' asMap M.empty $ nodes

moveFromTo :: Node -> Node -> (Node, Node)
moveFromTo from to = ((Node floc fsize tused (fsize - tused)), (Node tloc tsize fused (tsize - fused)))
    where (Node floc fsize fused favail) = from
          (Node tloc tsize tused tavail) = to

moveFromTo' :: Coord -> Coord -> M.Map Coord Node -> M.Map Coord Node
moveFromTo' from to nodes = s''
    where 
        (from', to') = moveFromTo (nodes M.! from) (nodes M.! to)
        s' = M.insert to to' nodes
        s'' = M.insert from from' s'

isValidState :: State -> Bool
isValidState (State nodes _) = all f (M.elems nodes)
    where f (Node nloc nsize nused navail) = (nused <= nsize) && (nsize - nused == navail)

adjacent :: (Coord, Coord) -> Bool
adjacent ((x1, y1), (x2, y2)) | x1 == x2 && (abs (y1 - y2) == 1) = True
adjacent ((x1, y1), (x2, y2)) | y1 == y2 && (abs (x1 - x2) == 1) = True
adjacent _ = False

nextEntries :: State -> [State]
nextEntries (State nodes goal) = states
    where 
        states = map change viable
        change (from, to) = State (moveFromTo' from to nodes) (if from == goal then to else goal)
        viable = filter adjacent . viablePairs $ (M.elems nodes)

isTarget :: State -> Bool
isTarget (State _ goal) = goal == (0, 0)

stateString :: State -> [String]
stateString (State nodes _) = map f (M.elems nodes)
    where 
        f n = show (used n) ++ "/" ++ show (size n)

main :: IO ()
main = do
    nodes <- catMaybes . map parse . lines <$> readFile "input.txt"

    -- part 1
    -- print $ length $ viablePairs nodes

    -- part 2
    -- let f n = (loc n, n)
    -- let initNodes = M.fromList . map f $ nodes
    -- let initState = State initNodes (2, 0)

    -- print $ bfs initState isTarget nextEntries
    --      ((a1,b1))((a2,b2))
    -- 
    print $ (Pair ((0, 1),  (1, 2)) == Pair ((1, 2),  (0, 1)))
    print $ (Pair ((0, 1),  (1, 2)) == Pair ((1, 1),  (0, 1)))
    print $ (Pair ((0, 1),  (1, 2)) == Pair ((0, 1),  (1, 2)))


    -- let f n = (loc n, n)
    -- let initNodes = M.fromList . map f $ nodes
    -- let initState = State initNodes (33, 0)

    -- print $ bfs initState isTarget nextEntries    

