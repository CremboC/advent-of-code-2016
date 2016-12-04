import Data.Char
import Data.List
import Data.Ord
import Data.Monoid

type Room = (String, Int, String)

roomify :: String -> Room
roomify r = (room, sector, checksum)
    where
        room = init rm -- the init removes the last dash before the digits
        sector = read . filter isDigit $ xs
        checksum = filter isAlpha . dropWhile (/='[') $ xs
        (rm, xs) = break isDigit r

-- The function mconcat scans through the list and obtains the first
-- non-EQ occurence (or EQ if all elements are EQ and thus both pairs are considered equal).
srt lst1 lst2 = mconcat [compare (length lst2) (length lst1), compare (head lst1) (head lst2)]

mkChecksum :: String -> String
mkChecksum room = chsm
    where
        chsm = map head cms
        cms = take 5 . sortBy srt . group . sort . filter isAlpha $ room

isRoom :: Room -> Bool
isRoom (room, _, checksum) = checksum == mkChecksum room

rotate :: Int -> Char -> Char
rotate amount '-' = ' '
rotate amount char
    | amount == 0 = char
    | succ char == succ 'z' = rotate (amount - 1) 'a'
    | otherwise = rotate (amount - 1) (succ char)

maxJump = ord 'z' - ord 'a'

decipher :: Room -> String
decipher (room, sector, _) = map (rotate $ sector `quot` maxJump) room

main = do
    input <- readFile "input.txt"
    let rooms = filter isRoom . map roomify . lines $ input
    -- part 1
    print $ foldl (\acc (_, sector, _) -> acc + sector) 0 rooms
    -- part 2
    print $ case find (("north" `isInfixOf`) . decipher) rooms of
        Just room@(name, sector, checksum) -> decipher room ++ " is at sector " ++ show sector
        Nothing -> "not found"