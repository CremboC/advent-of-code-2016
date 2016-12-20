{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}

module Search.BFS (bfs, bfsOfDistance) where
    
import qualified Data.Set as Set

type Node a = (a, Int)
type NextEntries a = (a -> [a])

bfs :: (Ord a) => a -> a -> NextEntries a -> Maybe (Node a)
bfs start target nextEntries = search [(start, 0)] Set.empty
    where
        search (node:xs) seen = if val == target
                then Just node
                else search (xs ++ next) (Set.insert val seen)
            where
                (val, distance) = node
                xsVals = map fst xs
                next =  map f . filter (not . isSeen) $ (nextEntries val)
                isSeen n = or [Set.member n seen, n `elem` xsVals]
                f a = (a, succ distance)
        search _ _ = Nothing

bfsOfDistance :: (Ord a) => a -> Int -> NextEntries a -> [a]
bfsOfDistance start target nextEntries = search [(start, 0)] Set.empty
    where
        stop = succ target
        search (node:xs) seen = if distance == stop
                then Set.toList seen
                else search (xs ++ next) (Set.insert val seen)
            where
                (val, distance) = node
                xsVals = map fst xs
                next =  map f . filter (not . isSeen) $ (nextEntries val)
                isSeen n = or [Set.member n seen, n `elem` xsVals]
                nextDistance = succ distance
                f a = (a, nextDistance)
        search _ _ = []