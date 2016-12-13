{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}

module Search.AStar (astar) where

import qualified Data.Map.Strict as M
import Data.Foldable (minimumBy)
import Data.Ord (comparing)

type Cost = Integer
data Status = Open | Closed | Unvisited deriving Show
-- path cost , node status, parent direction
data Node = Node Cost Status deriving Show
type World a = M.Map a Node
type NextEntries a = (a -> [a])
type Heuristic a = (a -> Integer)

astar :: forall a. Ord a => a -> a -> NextEntries a -> Heuristic a -> (a, Integer)
astar start target nextEntries heuristic = loop (open start M.empty)
    where
        loop :: World a -> (a, Integer)
        loop world | null world = (start, 0)
        loop world = if val == target
            then (val, getCost mnode)
            else loop world'
            where
                (val, mnode) = findOpenMinCost world
                world' = foldl f (close val world) (nextEntries val)
                f tworld val' = updateByStatus val' mnode tworld

        findOpenMinCost :: World a -> (a, Node)
        findOpenMinCost world = minc
            where
                minc = minimumBy (comparing f) (M.toList opened)
                f (val, (Node cost _)) = cost + heuristic val
                opened = M.filter isOpen world

        updateByStatus :: a -> Node -> World a -> World a
        updateByStatus val (Node pcost _) world = f tstatus
            where
                f Open = if tcost > newCost
                    then updateNode f' val world
                    else world
                    where
                        f' _ = Just $ (Node newCost tstatus)
                f Unvisited = updateNode f' val world
                    where
                        f' Nothing = Just $ (Node newCost Open)
                        f' (Just (Node _ _)) = Just $ (Node newCost Open)
                f Closed = world
                (Node tcost tstatus) = getNode val world
                newCost = succ pcost

        updateNode :: (Maybe Node -> Maybe Node) -> a -> World a -> World a
        updateNode modifier coord world = M.alter modifier coord world

        getNode :: a -> World a -> Node
        getNode coord world = M.findWithDefault d coord world
            where d = (Node 0 Unvisited)

        open :: a -> World a -> World a
        open c world = M.alter f c world
            where f _ = Just $ Node 0 Open

        close :: a -> World a -> World a
        close c world = M.alter f c world
            where
                f Nothing = Nothing
                f (Just (Node cost _)) = Just $ (Node cost Closed)


getCost :: Node -> Integer
getCost (Node cost _) = cost

isOpen :: Node -> Bool
isOpen (Node _ Open) = True
isOpen _ = False