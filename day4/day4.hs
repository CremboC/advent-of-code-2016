import Data.Char
import Data.List
import Data.Ord
import Data.Monoid

type Room = (String, Int, String)

roomify :: String -> Room
roomify r = (room, sector, checksum)
    where
        room = filter (isAlpha) rm
        sector = read $ filter (isDigit) xs
        checksum = filter isAlpha $ dropWhile (/='[') xs
        (rm, xs) = break (isDigit) r

-- The function mconcat simply scans through the list and obtains the first
-- non-EQ occurence (or EQ if all elements are EQ and thus both pairs are considered equal).
srt (a1, a2) (b1, b2) = mconcat [compare b1 a1, compare a2 b2]

checksum' :: String -> String
checksum' room = chsm
    where
        chsm = map (snd) cms
        cms = (take 5 . sortBy (srt) . map (\g -> (length g, head g)) . group . sort) $ room

isRoom :: Room -> Bool
isRoom (room, _, checksum) = checksum == checksum' room

main = do
    input <- readFile "input.txt"
    let rooms = map roomify (lines input)
    print $ foldl (\acc (_, sector, _) -> acc + sector) 0 $ filter isRoom rooms










