{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}

module Search.BFS (bfs, bfsOfDistance) where

import qualified Data.Sequence as S
import Data.Sequence (
        (><),
        fromList,
        viewl,
        ViewL((:<), EmptyL)
    )
import qualified Data.Set as Set

type Node a = (a, Int)
type NextEntries a = (a -> [a])

bfs :: (Ord a) => a -> a -> NextEntries a -> Node a
bfs start target nextEntries = search (S.singleton (start, 0)) Set.empty
    where
        search (viewl -> EmptyL) _ = (start, 0)
        search (viewl -> (s :< xs)) seen =
            if val == target 
                then (val, distance) 
                else search (xs >< fromList next) (Set.insert val seen)
            where
                (val, distance) = s
                next = fmap f . filter (not . isSeen) $ (nextEntries val)
                isSeen a = Set.member a seen
                f a = (a, succ distance)
        search _ _ = (start, 0)

bfsOfDistance :: forall a. (Ord a, Show a) => a -> Int -> NextEntries a -> [Node a]
bfsOfDistance start target nextEntries = search (S.singleton (start, 0)) Set.empty
    where
        search (viewl -> EmptyL) _ = []
        search (viewl -> (node :< xs)) seen =
            if distance == succ target
                then Set.toList seen
                else search (xs >< fromList next) (Set.insert node seen)
            where
                (val, distance) = node
                next =  filter (not . isSeen) . fmap f $ (nextEntries val)
                isSeen node = Set.member node seen
                f a = (a, succ distance)
        search _ _ = []