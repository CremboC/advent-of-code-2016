module Main where

import Data.List (find, elemIndex)

type State = [(Int, Int)]

input :: State -- (end, current)
input = [(13, 11), (5, 0), (17, 11), (3, 0), (7, 2), (19, 17)]

input' :: State 
input' = [(13, 11), (5, 0), (17, 11), (3, 0), (7, 2), (19, 17), (11, 0)]

tick :: State -> State
tick state = fmap f state
    where f (end, current) = (end, (cycle [0..(end - 1)]) !! (succ current)) 

-- copied from http://codereview.stackexchange.com/a/54110/63029
chunk :: Int -> [a] -> [[a]]
chunk n xs = if length chunk' < n 
    then [] 
    else (chunk' : chunk n (tail xs))
    where chunk' = take n xs

loop :: State -> [State]
loop state = state : (loop (tick state))

match :: [[(Int, Int)]] -> Bool
match lst = all f . zip [0..] $ lst
    where f (i, s) = 0 == (snd $ s !! i)

main :: IO ()
main = do
    -- part 1
    let chunked = chunk 6 . loop $ (tick input)
    print $ case find match chunked of
        Nothing -> Nothing
        (Just s) -> s `elemIndex` chunked

    -- -- part 2
    let chunked' = chunk 7 . loop $ (tick input')
    print $ case find match chunked' of
        Nothing -> Nothing
        (Just s) -> s `elemIndex` chunked'

