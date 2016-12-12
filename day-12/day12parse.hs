module Day12Parse (
        parseAdvent,
        Instruction(Copy, Jump, Inc, Dec),
        Expr(Literal, Register),
        Name
    ) where

import Text.Megaparsec
import Text.Megaparsec.String
import Data.Maybe
import Control.Applicative
import qualified Text.Megaparsec.Lexer as L

type Name = Char
data Expr = Literal Int | Register Name deriving Show
data Instruction = Copy Expr Expr | Jump Expr Int | Inc Expr | Dec Expr deriving Show

nf = (\d -> Literal (fromIntegral d)) <$> L.integer
rf = (\r -> Register r) <$> letterChar

cpy :: Parser Instruction
cpy = Copy <$> (string "cpy " *> choice [nf, rf]) <*> (space *> choice [nf, rf])

jnz :: Parser Instruction
jnz' e1 neg amm = (Jump e1 (fromIntegral $ if isJust neg then (0 - amm) else amm))
jnz = jnz' <$> (string "jnz " *> choice [nf, rf]) <*> (space *> optional (char '-')) <*> L.integer

inc :: Parser Instruction
inc = Inc <$> (string "inc " *> rf)

dec :: Parser Instruction
dec = Dec <$> (string "dec " *> rf)

parseAdvent :: String -> Maybe Instruction
parseAdvent = parseMaybe (cpy <|> jnz <|> inc <|> dec)