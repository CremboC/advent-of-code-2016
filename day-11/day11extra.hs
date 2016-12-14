module Day11Extra where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

listDifference :: (Ord a) => [a] -> [a] -> [a]
listDifference a b = go initHist a
  where
    initHist = Map.fromListWith (+) [ (x, 1 :: Int) | x <- b ]

    go _    []     = []
    go hist (x:xs) = case Map.lookup x hist of
      Just n | n > 0 ->     go (Map.insert x (n-1) hist) xs
      _              -> x : go hist                      xs