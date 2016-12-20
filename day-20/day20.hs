{-# LANGUAGE TupleSections, ViewPatterns #-}

module Main where

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.List (sortBy)

import qualified Data.Sequence as S

type Range = (Int, Int)

signedInt :: ParsecT Dec String a Int
signedInt = fromIntegral <$> (L.signed space L.integer)

range :: Parser Range
range = (,) <$> signedInt <*> (char '-' *> signedInt)

findMin :: Int -> S.Seq Range -> Int
findMin x (S.viewl -> ((from, to) S.:< xs)) = if x <= to && x >= from 
    then findMin (succ to) xs else findMin x xs
findMin x _ = x

main :: IO ()
main = do
    ranges <- catMaybes . map (parseMaybe range) . lines <$> readFile "input.txt"
    print $ findMin 0 . S.fromList . sortBy (comparing fst) $ ranges