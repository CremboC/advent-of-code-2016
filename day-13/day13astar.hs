{-# LANGUAGE ViewPatterns #-}

module Day13Astar (
        astar,
        Coord,
        Status(Open, Closed, Unvisited),
        Direction(N, S, W, E),
        Node,

        SafetyChecker
    ) where

import qualified Data.Map.Strict as M
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Debug.Trace (traceShow)

type Coord = (Integer, Integer)
type Cost = Integer
type SafetyChecker = Coord -> Bool
data Status = Open | Closed | Unvisited deriving Show
data Direction = N | S | W | E | None deriving Show
-- path cost , node status, parent direction
data Node = Node Cost Status Direction deriving Show
type World = M.Map Coord Node

astar :: Coord -> Coord -> SafetyChecker -> Integer
astar start end checker = loop (open start M.empty) 1 end
    where
        loop :: World -> Integer -> Coord -> Integer
        loop _ 0 _ = error "Not found"
        loop world opened end = if mcoord == end 
            then getCost mnode 
            else loop world'' opened end
            where
                -- find the lowest cost node
                (mcoord, mnode) = findOpenMinCost world end
                -- close it
                world' = close mcoord world
                -- check all neighbours around it
                (x, y) = mcoord
                world'' = 
                    checkNeighbor (x, pred y) mnode None .
                    checkNeighbor (x, succ y) mnode None .
                    checkNeighbor (succ x, y) mnode None .
                    checkNeighbor (pred x, y) mnode None $ world'

        findOpenMinCost :: World -> Coord -> (Coord, Node)
        findOpenMinCost world end = minc
            where
                minc = minimumBy (comparing f) (M.toList open)
                f (coord, (Node cost _ _)) = cost + manhattan coord end
                open = M.filter isOpen world

        isSafe :: Coord -> Bool
        isSafe (x, y) = and [x >= 0, y >= 0]

        checkNeighbor :: Coord -> Node -> Direction -> World -> World
        checkNeighbor new parent _ world | isSafe new && checker new = 
            updateByStatus new parent world
        checkNeighbor new _ _ world = world

        updateByStatus :: Coord -> Node -> World -> World
        updateByStatus coord (Node pcost _ _) world = f tstatus
            where 
                f Open = if tcost > newCost 
                    then updateNode f' coord world
                    else world
                    where 
                        f' _ = Just $ (Node newCost tstatus None)
                f Unvisited = updateNode f' coord world
                    where 
                        f' Nothing = Just $ (Node newCost Open None)
                        f' (Just (Node c s d)) = Just $ (Node newCost Open d)
                f Closed = world
                targetNode@(Node tcost tstatus _) = getNode coord world
                newCost = succ pcost
                updateCost ncost (Node _ status direction) = (Node ncost status direction)

        updateNode :: (Maybe Node -> Maybe Node) -> Coord -> World -> World
        updateNode modifier coord world = M.alter modifier coord world

manhattan :: Coord -> Coord -> Integer
manhattan (x1, y1) (x2, y2) = x' + y'
    where 
        x' = if x1 > x2 then x1 - x2 else x2 - x1
        y' = if y1 > y2 then y1 - y2 else y2 - y1

getCost :: Node -> Integer
getCost (Node cost _ _) = cost

getNode :: Coord -> World -> Node
getNode coord world = M.findWithDefault d coord world
    where d = (Node 0 Unvisited None)

open :: Coord -> World -> World
open c world = M.alter f c world
    where f _ = Just $ Node 0 Open None

close :: Coord -> World -> World
close c world = M.alter f c world
    where 
        f Nothing = Nothing
        f (Just (Node cost status direction)) = Just $ (Node cost Closed direction)

isOpen :: Node -> Bool
isOpen (Node _ Open _) = True
isOpen _ = False