{-# LANGUAGE TupleSections #-}
module Main where

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Data.Maybe (catMaybes)
import Data.List (foldl', sortBy, nubBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- Filesystem              Size  Used  Avail  Use%
-- /dev/grid/node-x0-y0     91T   71T    20T   78%
type Coord = (Int, Int)
data Node = Node { loc :: Coord, size :: Int, used :: Int, available :: Int, use' :: Int } deriving (Show, Eq, Ord)

parse :: String -> Maybe Node
parse = parseMaybe (node)
    where
        node = f <$> (string "/dev/grid/node-x" *> int) <*> (string "-y" *> int) <*> fsspace <*> fsspace <*> fsspace <*> fsspace
        f x y = Node (x, y)
        fsspace = (skipMany spaceChar) *> int <* (choice [char 'T', char '%'])
        int = fromIntegral <$> L.integer :: ParsecT Dec String a Int

asMap :: M.Map Int [Node] -> Node -> M.Map Int [Node]
asMap m n = M.alter f (available n) m
    where f (Just s) = Just (n : s)
          f Nothing  = Just [n]

pairs :: M.Map Int [Node] -> Node -> [(Node, Node)]
pairs m n = map (n,) . concat . M.elems . M.dropWhileAntitone f $ m
    where f k = (used n) > k

tupleEq :: (Node, Node) -> (Node, Node) -> Bool
tupleEq (a1, b1) (a2, b2) = or [a1 == a2 && b1 == b2, a1 == b2 && b1 == a2]

same :: (Node, Node) -> Bool
same (a, b) = a == b

main = do
    nodes <- catMaybes . map parse . lines <$> readFile "input.txt"
    let byAvailable = foldl' asMap M.empty $ nodes
    print $ length . filter (not . same) . nubBy tupleEq . concat . map (pairs byAvailable) . filter ((/= 0) . used) $ nodes