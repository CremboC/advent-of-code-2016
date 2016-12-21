module Day21Parse (
    parseAdvent, 
    Instruction(Reverse, SwapL, SwapP, RotateD, RotateP, Move), 
    Direction(L, R)
    ) where

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

data Direction = L | R deriving (Show, Eq)
data Instruction = Reverse Int Int | 
                SwapP Int Int | 
                SwapL Char Char |
                RotateD Direction Int |
                RotateP Char |
                Move Int Int
                deriving Show

int :: ParsecT Dec String a Int
int = fromIntegral <$> L.integer

-- reverse positions 0 through 6
reversei :: Parser Instruction
reversei = Reverse <$> (string "reverse positions " *> int) <*> (string " through " *> int)

-- swap position 0 with position 1
swapp :: Parser Instruction
swapp = SwapP <$> (string "swap position " *> int) <*> (string " with position " *> int)

-- swap letter e with letter g
swapl :: Parser Instruction
swapl = SwapL <$> (string "swap letter " *> letterChar) <*> (string " with letter " *> letterChar)

dir :: Parser Direction
dir = f <$> choice [string "right", string "left"]
    where f "left" = L; f _ = R
          
-- rotate left 2 steps
-- rotate right 0 steps
rotated :: Parser Instruction
rotated = RotateD <$> (string "rotate " *> dir) <*> ((space *> int) <* string " step") <* many letterChar

-- rotate based on position of letter f
rotatep :: Parser Instruction
rotatep = RotateP <$> (string "rotate based on position of letter " *> asciiChar)

-- move position 3 to position 6
move :: Parser Instruction
move = Move <$> (string "move position " *> int) <*> (string " to position " *> int)

parseAdvent :: String -> Maybe Instruction
parseAdvent = parseMaybe (reversei <|> swapp <|> swapl <|> rotatep <|> rotated <|> move)