{-# LANGUAGE TupleSections, ViewPatterns #-}

module Main where

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.List (sortBy, find)
import Data.Range.Range
import Data.Range.NestedRange
import Data.Word

import qualified Data.Sequence as S

type Rng = (Int, Int)

signedInt :: ParsecT Dec String a Int
signedInt = fromIntegral <$> (L.signed space L.integer)

range :: Parser Rng
range = (,) <$> signedInt <*> (char '-' *> signedInt)

findMin :: Int -> S.Seq Rng -> Int
findMin x (S.viewl -> ((from, to) S.:< xs)) = if x <= to && x >= from 
    then findMin (succ to) xs else findMin x xs
findMin x _ = x

main :: IO ()
main = do
    ranges <- catMaybes . map (parseMaybe range) . lines <$> readFile "input.txt"
    let bound = fromIntegral (maxBound :: Word32) :: Int

    -- part 1
    print $ findMin 0 . S.fromList . sortBy (comparing fst) $ ranges

    -- part 2
    let f (a, b) = [SpanRange a b]
    let rngs = map f ranges

    let merged = foldl (union) (head rngs) rngs
    let valid (SingletonRange _) = True
        valid _ = False
    print $ length . filter valid $ invert merged


