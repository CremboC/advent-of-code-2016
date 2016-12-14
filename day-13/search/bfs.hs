{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}

module Search.BFS (bfs, bfsOfDistance) where

import Data.Set (Set)
import qualified Data.Set as Set

type Node a = (a, Int)
type NextEntries a = (a -> [a])

bfs :: (Ord a) => a -> a -> NextEntries a -> Node a
bfs start target nextEntries = search [(start, 0)] Set.empty
    where
        search (node:xs) seen = if val == target 
                then node
                else search (xs ++ next) (Set.insert val seen)
            where
                (val, distance) = node
                xsVals = fmap fst xs
                next =  fmap f . filter (not . isSeen) $ (nextEntries val)
                isSeen node = or [Set.member node seen, node `elem` xsVals]
                f a = (a, succ distance)
        search _ _ = (start, 0)

bfsOfDistance :: (Ord a) => a -> Int -> NextEntries a -> [a]
bfsOfDistance start target nextEntries = search [(start, 0)] Set.empty
    where
        stop = succ target
        search (node:xs) seen = if distance == stop
                then Set.toList seen
                else search (xs ++ next) (Set.insert val seen)
            where
                (val, distance) = node
                xsVals = fmap fst xs
                next =  fmap f . filter (not . isSeen) $ (nextEntries val)
                isSeen node = or [Set.member node seen, node `elem` xsVals]
                nextDistance = succ distance
                f a = (a, nextDistance)
        search _ _ = []