module Main where

import qualified Data.Sequence as S

solve :: Int -> Int
solve n = f (S.fromList [1..n])
    where 
        f sq | length sq == 1 = let (a S.:< _) = S.viewl sq in a
        f sq | otherwise = f (xs S.|> a)
            where 
                (a S.:< _) = S.viewl . S.take 2 $ sq
                xs = S.drop 2 sq

div' :: Int -> Int -> Float
div' a b = (fromIntegral a) / (fromIntegral b)

solve' :: Int -> Int
solve' n = f (S.fromList [1..n])
    where 
        f sq | length sq == 1 = let (a S.:< _) = S.viewl sq in a
        f sq | otherwise = f (xs S.|> a)
            where
                (a S.:< _) = S.viewl sq
                (_ S.:< xs) = S.viewl . S.deleteAt across $ sq
                f' = if odd . length $ sq then pred . ceiling else ceiling
                across = f' $ (length sq) `div'` 2 :: Int

main :: IO ()
main = do
    -- part 1
    print $ solve 3001330

    -- part 2
    print $ solve' 3001330
