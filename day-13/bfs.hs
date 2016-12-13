{-# LANGUAGE ViewPatterns #-}

module Search.BFS (bfs) where

import qualified Data.Sequence as S
import Data.Sequence (
        (><), (<|),
        fromList,
        viewl,
        ViewL((:<), EmptyL)
    )
import qualified Data.Set as Set

data Node a = Node a Int | Empty deriving Show
type Target a = (a -> Bool)
type NextEntries a = (a -> [a])

bfs :: (Show a, Ord a) => a -> Target a -> NextEntries a -> Node a
bfs root checker nextEntries = search (S.singleton (Node root 0)) Set.empty
    where
        search (viewl -> EmptyL) _ = Empty
        search (viewl -> (s :< xs)) seen =
            if checker val then s else search (xs >< fromList next) (Set.insert val seen)
            where
                (Node val distance) = s
                next = fmap f . filter (not . isSeen) $ (nextEntries val)
                isSeen a = Set.member a seen
                f a = Node a (succ distance)