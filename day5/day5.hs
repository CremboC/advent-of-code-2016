import Crypto.Hash
import Data.ByteString.Char8 (pack)
import Data.List
import Data.Char
import Data.Ord
import qualified Data.Map.Strict as Map

input = "abbhdwsy"
md5 x = hash x :: Digest MD5
md5' x = show . md5 . pack $ x

crack :: Int -> String -> String
crack index password
    | length password == 8 = password
    | otherwise = if "00000" `isPrefixOf` hashed
        then crack (succ index) (password ++ [hashed !! 5])
        else crack (succ index) password
    where hashed = md5' $ input ++ show index

crack' :: Int -> Map.Map Int Char -> Map.Map Int Char
crack' index password
    | length password == 8 = password
    | ("00000" `isPrefixOf` hashed) && (pos `Map.notMember` password) && (pos `elem` [0..7]) =
        crack' (succ index) $ Map.insert pos char password
    | otherwise = crack' (succ index) password
    where
        (pos, char) = (digitToInt $ hashed !! 5, hashed !! 6)
        hashed = md5' $ input ++ show index

main = do
    -- part 1
    print $ crack 0 ""
    -- part 2
    print $ map snd $ sortBy (comparing fst) $ Map.toList $  crack' 0 Map.empty
