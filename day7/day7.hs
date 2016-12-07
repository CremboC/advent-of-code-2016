import Data.List
import Data.List.Split

parse :: String -> [[String]]
parse ip = transpose . chunksOf 2 . concatMap (splitOn "]") . splitOn "[" $ ip

hasAbba :: String -> Bool
hasAbba "" = False
hasAbba (a:b:c:d:xs) = 
    if a == d && b == c && a /= b 
        then True 
        else hasAbba (b:c:d:xs)
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

mkBab :: String -> String
mkBab (a:b:_) = b:a:b:[]

supportsAba :: ([String], [String]) -> Bool
supportsAba (outs, ins) = (/= 0) . length . filter abaHasBab $ abas
    where
        abas = concatMap (`mkAba` []) outs
        abaHasBab aba = (/= 0) . length . filter ((mkBab aba) `isInfixOf`) $ ins

main :: IO ()
main = do
    ips <- map (\(a:b) -> (a, concat b)) . map parse . lines <$> readFile "input.txt"

    -- part 1
    print $ length . filter supportsAbba $ ips

    -- part 2
    print $ length . filter supportsAba $ ips