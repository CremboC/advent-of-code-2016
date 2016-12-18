{-# LANGUAGE ScopedTypeVariables #-}

module Day17Search (bfs, bfsMax) where

import qualified Data.Set as Set

type Node a = (a, Int)
type NextEntries a = (a -> [a])
type IsTarget a = (a -> Bool)

bfs :: (Ord a) => a -> IsTarget a -> NextEntries a -> Maybe (Node a)
bfs start isTarget nextEntries = search [(start, 0)] Set.empty
    where
        search (node:xs) seen = if isTarget val
                then Just node
                else search (xs ++ next) (Set.insert val seen)
            where
                (val, distance) = node
                xsVals = map fst xs
                next =  map f . filter (not . isSeen) $ (nextEntries val)
                isSeen n = or [Set.member n seen, n `elem` xsVals]
                f a = (a, succ distance)
        search _ _ = Nothing

bfsMax :: (Ord a) => a -> IsTarget a -> NextEntries a -> [Node a]
bfsMax start isTarget nextEntries = search [(start, 0)] Set.empty
    where
        search (node:xs) seen = if isTarget val
            then node : search xs seen
            else search (xs ++ next) (Set.insert val seen)
            where
                (val, distance) = node
                xsVals = map fst xs
                next =  map f . filter (not . isSeen) $ (nextEntries val)
                isSeen n = or [Set.member n seen, n `elem` xsVals]
                f a = (a, succ distance)
        search _ _ = []