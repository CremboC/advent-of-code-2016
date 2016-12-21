module Main where

import Data.Maybe (catMaybes, fromMaybe)
import Data.Foldable (toList)
import Data.List (permutations, find)
import qualified Data.Sequence as S
import Day21Parse

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) . drop n . cycle $ xs

exec :: Instruction -> S.Seq Char -> S.Seq Char
exec (Reverse from to) str = s S.>< (S.reverse subject) S.>< end
    where
        (subject, end) = S.splitAt (to - from + 1) ss
        (s, ss) = S.splitAt from str
exec (SwapP x y) str = S.update y x' . S.update x y' $ str
    where
        x' = str `S.index` x
        y' = str `S.index` y
exec (SwapL x y) str = fmap f str
    where
        f c | c == x = y
            | c == y = x
            | otherwise = c
exec (RotateD L x) str = S.fromList . rotate x . toList $ str
exec (RotateD R x) str = S.fromList . rotate (length str - x) . toList $ str
exec (Move x y) str = S.insertAt (y) (S.index str x) str'
    where str' = S.deleteAt x str
exec (RotateP x) str = S.fromList . rotate rot . toList $ str
    where 
        rot = (m (length str) (rright)) - rright
        rright = index + 1 + if index >= 4 then 1 else 0
        index = fromMaybe 0 (x `S.elemIndexL` str)
        m i g = if i > g then i else m (i * 2) g

run :: [Instruction] -> S.Seq Char -> S.Seq Char
run [] str = str
run (i:is) str = run is (exec i str)

main :: IO ()
main = do
    instructions <- catMaybes . map parseAdvent . lines <$> readFile "input.txt"

    -- part 1
    print $ run instructions (S.fromList "abcdefgh")

    -- part 2
    let target = S.fromList "fbgdceah"
    print $ find ((== target) . run instructions . S.fromList) . permutations $ "fbgdceah"
