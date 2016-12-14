module Main where

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Base16 as BS16
import Crypto.Hash.MD5 as MD5
import Data.List.Extra
import Data.Trie (Trie)
import qualified Data.Trie as T
import Data.Maybe

import Debug.Trace

type Memo = Trie String

input :: String
input = "qzyelonm"

hasThree :: Eq a => [a] -> Maybe a
hasThree (a:b:c:xs) = if a == b && b == c then Just a else hasThree (b:c:xs)
hasThree _ = Nothing

five :: [a] -> [a]
five x = concat $ replicate 5 x

hasFiveOf :: Eq a => [a] -> [a] -> Bool
hasFiveOf x xs = (five x) `isInfixOf` xs

md5 :: String -> String
md5 x = unpack . BS16.encode . MD5.hash . pack $ x

megaMd5 :: String -> String
megaMd5 x = foldl acc x [0..2016]
    where acc val _ = md5 val

findOrUpdate :: Memo -> String -> (String, Memo)
findOrUpdate memo key = (value, memo')
    where
        key' = pack key
        memoizedVal = T.lookup key' memo
        wasMemoized = isJust memoizedVal
        value = case memoizedVal of
            Nothing -> megaMd5 key
            (Just val) -> val
        memo' = if wasMemoized then memo else (T.insert key' value memo)

quickSearch :: Memo -> String -> Int -> Int -> (Bool, Memo)
quickSearch memo _ index eIndex | index == eIndex = (False, memo)
quickSearch memo char index eIndex = 
    if hasFiveOf char value 
        then (True, memo')
        else quickSearch memo' char (succ index) eIndex
    where
        (value, memo') = findOrUpdate memo (input ++ show index)

exec :: Memo -> Int -> Int -> [(Int, String)]
exec _ _ 64 = []
exec memo index len = if found 
    then traceShow (len, index, value) $ (index, value) : (exec memo'' (succ index) (succ len))
    else exec memo'' (succ index) len
    where
        current = input ++ show index
        (value, memo') = findOrUpdate memo current
        repeating = hasThree value
        (found, memo'') = case repeating of
            Nothing -> (False, memo')
            (Just r) -> quickSearch memo' [r] (succ index) (index + 1000)

main :: IO ()
main = do
    -- both parts, need to change (md5|megaMd5) for (part 1|part 2)
    print $ fst . last $ exec T.empty 0 0