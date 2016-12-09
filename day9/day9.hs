{-# LANGUAGE ViewPatterns #-}

import Data.List
import Data.Char
import Data.List.Split

pattern :: String -> Maybe (Int, Int, String, String)
pattern str | not $ "(" `isInfixOf` str = Nothing
pattern str = Just (a, b, start, tail next)
    where
        [a, b] = map (read :: String -> Int) . map (filter isDigit) . splitOn "x" $ pattern
        (pattern, next) = break (==')') rest
        (start, rest) = break (=='(') str

decompress :: String -> String
decompress "" = ""
decompress (pattern -> Just (a, b, start, next)) = start ++ x' ++ decompress (drop a next)
    where x' = concat . replicate b . take a $ next
decompress str@(pattern -> Nothing) = str

decompress' :: String -> Int
decompress' "" = 0
decompress' (pattern -> Just (a, b, start, next)) = length start + sublength + decompress' (drop a next)
    where 
        sublength = b * (decompress' . take a $ next)
decompress' str@(pattern -> Nothing) = length str

main :: IO ()
main = do
    input <- readFile "input.txt"
    -- part 1
    print $ length . decompress $ input
    -- part 2
    print $ decompress' input
    