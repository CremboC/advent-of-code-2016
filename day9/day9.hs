{-# LANGUAGE ViewPatterns #-}

import Data.List (isInfixOf)
import Data.Char (isDigit)
import Data.List.Split (splitOn)

pattern :: String -> Maybe (Int, Int, String, String)
pattern str | not $ "(" `isInfixOf` str = Nothing
pattern str = Just (a, b, start, tail next)
    where
        [a, b] = map (read :: String -> Int) . map (filter isDigit) . splitOn "x" $ pattern
        (pattern, next) = break (==')') rest
        (start, rest) = break (=='(') str

decompress :: Bool -> String -> Int
decompress _ "" = 0
decompress full (pattern -> Just (a, b, start, next)) = length start + sublength + decompress full (drop a next)
    where sublength | full     = b * (decompress full . take a $ next)
                    | not full = b * (length . take a $ next)
decompress _ str@(pattern -> Nothing) = length str

main :: IO ()
main = do
    input <- readFile "input.txt"
    -- part 1
    print $ decompress False $ input
    -- part 2
    print $ decompress True $ input
    