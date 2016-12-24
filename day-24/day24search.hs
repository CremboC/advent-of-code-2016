{-# LANGUAGE ScopedTypeVariables #-}

module Day24Search(astar) where

import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Foldable (minimumBy)
import Data.Ord (comparing)

type Cost = Int
data Status = Open | Closed | Unvisited deriving Show
-- path cost , node status, parent
data Node a = Node Cost Status a deriving Show
type World a = Map a (Node a)
type NextEntries a = (Seq a -> Seq a)
type Heuristic a = (a -> Int)

astar :: forall a. Ord a => a -> a -> NextEntries a -> Heuristic a -> Seq a
astar start target nextEntries heuristic = loop (open start M.empty)
    where
        loop :: World a -> Seq a
        loop world | null world = S.empty
        loop world = if val == target
            then path world target
            else loop world'
            where
                (val, mnode) = findOpenMinCost world
                currentPath = path world val
                world' = foldl f (close val world) (nextEntries currentPath)
                f tworld val' = updateByStatus val' (val, mnode) tworld

        path :: World a -> a -> Seq a
        path world end = S.reverse . (end S.<|) . path' $ end
            where path' current = if current == start then S.empty else parent S.<| (path' parent) 
                    where (Node _ _ parent) = world M.! current

        findOpenMinCost :: World a -> (a, Node a)
        findOpenMinCost world = minc
            where
                minc = minimumBy (comparing f) (M.toList opened)
                f (val, (Node cost _ _)) = cost + heuristic val
                opened = M.filter isOpen world

        updateByStatus :: a -> (a, Node a) -> World a -> World a
        updateByStatus val (pval, (Node pcost _ _)) world = f tstatus
            where
                f Open = if tcost > newCost
                    then updateNode f' val world
                    else world
                    where f' _ = Just $ (Node newCost tstatus pval)
                f Unvisited = updateNode f' val world
                    where f' _ = Just $ (Node newCost Open pval)
                f Closed = world
                (Node tcost tstatus _) = getNode val world
                newCost = succ pcost

        updateNode :: (Maybe (Node a) -> Maybe (Node a)) -> a -> World a -> World a
        updateNode modifier val world = M.alter modifier val world

        getNode :: a -> World a -> Node a
        getNode val world = M.findWithDefault d val world
            where d = (Node 0 Unvisited val)

        open :: a -> World a -> World a
        open val world = M.alter f val world
            where f _ = Just $ Node 0 Open val

        close :: a -> World a -> World a
        close val world = M.alter f val world
            where
                f Nothing = Nothing
                f (Just (Node cost _ v)) = Just $ (Node cost Closed v)


-- getCost :: Node a -> Integer
-- getCost (Node cost _ _) = cost

isOpen :: Node a -> Bool
isOpen (Node _ Open _) = True
isOpen _ = False