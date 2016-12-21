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

parseAdvent :: String -> Maybe Instruction
parseAdvent = parseMaybe (reversei <|> swapp <|> swapl <|> rotatep <|> rotated <|> move)
    where
        reversei = Reverse <$> (string "reverse positions " *> int) <*> (string " through " *> int)
        swapp    = SwapP <$> (string "swap position " *> int) <*> (string " with position " *> int)
        swapl    = SwapL <$> (string "swap letter " *> letterChar) <*> (string " with letter " *> letterChar)
        dir      = f <$> choice [string "right", string "left"] where f "left" = L; f _ = R
        rotated  = RotateD <$> (string "rotate " *> dir) <*> (space *> int <* string " step" <* optional (char 's'))
        rotatep  = RotateP <$> (string "rotate based on position of letter " *> asciiChar)
        move     = Move <$> (string "move position " *> int) <*> (string " to position " *> int)
