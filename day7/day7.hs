{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
import Text.Regex.PCRE.Heavy
import Data.List
import Debug.Trace

regexOut = [re|\[[a-z]+\]|]
regexIn = [re|[a-z]+|]
regexAbba = [re|([a-z])([a-z])\2\1|]
regexAba = [re|([a-z])([a-z])\1|]

parse :: String -> ([String], [String])
parse ip = (matchOut, matchIn)
    where
        matchOut = split regexOut ip
        matchIn = map (filter (`notElem` ['[', ']'])) . map fst $ scan regexOut ip

hasAbba :: String -> Bool
hasAbba "" = False
hasAbba (a:b:c:d:xs) = if a == d && b == c && a /= b then True else hasAbba (b:c:d:xs)
hasAbba _ = False

supportsAbba :: ([String], [String]) -> Bool
supportsAbba (outs, ins) = validOuts && validIns
    where
        validOuts = any hasAbba outs
        validIns = not $ any hasAbba ins

mkAba :: String -> [String] -> [String]
mkAba "" found = found
mkAba (a:b:c:xs) found =
    if a == c && a /= b
        then mkAba (b:c:xs) (found ++ [a:b:a:[]])
        else mkAba (b:c:xs) found
mkAba _ found = found

supportsAba :: ([String], [String]) -> Bool
supportsAba (outs, ins) = (/= 0) . length . filter abaHasBab $ abas
    where
        abas = concatMap (`mkAba` []) outs
        abaHasBab aba = (/= 0) . length . filter ((mkBab aba) `isInfixOf`) $ ins
        mkBab (a:b:_) = b:a:b:[]

main :: IO ()
main = do
    ips <- map parse . lines <$> readFile "input.txt"
    -- part 1
    print $ length . filter supportsAbba $ ips

    -- part 2
    print $ length . filter supportsAba $ ips