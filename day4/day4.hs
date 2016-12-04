import Data.Char
import Data.List
import Data.Ord
import Data.Monoid
import Debug.Trace

type Room = (String, Int, String)

roomify :: String -> Room
roomify r = (room, sector, checksum)
    where
        room = init rm -- the init removes the last dash before the digits
        sector = (read . filter isDigit) xs
        checksum = (filter isAlpha . dropWhile (/='[')) xs
        (rm, xs) = break isDigit r

-- The function mconcat scans through the list and obtains the first
-- non-EQ occurence (or EQ if all elements are EQ and thus both pairs are considered equal).
srt (a1, a2) (b1, b2) = mconcat [compare b1 a1, compare a2 b2]

mkChecksum :: String -> String
mkChecksum room = chsm
    where
        chsm = map (snd) cms
        cms = (take 5 . sortBy (srt) . map (\g -> (length g, head g)) . group . sort . filter isAlpha) $ room

isRoom :: Room -> Bool
isRoom (room, _, checksum) = checksum == mkChecksum room

rotate :: Int -> Char -> Char
rotate amount '-' = ' '
rotate amount char
    | amount == 0 = char
    | otherwise = if succ char == succ 'z'
        then rotate (amount - 1) 'a'
        else rotate (amount - 1) (succ char)

maxJump = ord 'z' - ord 'a'

decipher :: Room -> String
decipher (room, sector, _) = map (rotate $ sector `quot` maxJump) room

main = do
    input <- readFile "input.txt"
    let rooms = filter isRoom $ map roomify $ lines input
    -- part 1
    print $ foldl (\acc (_, sector, _) -> acc + sector) 0 rooms
    -- part 2
    print $ filter (("north" `isInfixOf`) . decipher) rooms