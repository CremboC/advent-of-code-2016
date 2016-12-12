module Day12Parse (
        parseAdvent,
        Instruction(Copy, Jump, Inc, Dec),
        Expr(Literal, Register),
        Name
    ) where

import Text.Megaparsec
import Text.Megaparsec.String
import Data.Maybe
import qualified Text.Megaparsec.Lexer as L

type Name = Char
data Expr = Literal Int | Register Name deriving Show
data Instruction = Copy Expr Expr | Jump Expr Int | Inc Expr | Dec Expr deriving Show

nf = (\d -> Literal (fromIntegral d)) <$> L.integer
rf = (\r -> Register r) <$> letterChar

cpy :: Parser Instruction
cpy = do
    string "cpy "
    expr1 <- choice [nf, rf]
    space
    expr2 <- choice [nf, rf]
    return (Copy expr1 expr2)

jnz :: Parser Instruction
jnz = do
    string "jnz "
    expr1 <- choice [nf, rf]
    space
    neg <- optional (char '-')
    amount <- fromIntegral <$> L.integer
    let amount' = if isJust neg then (0 - amount) else amount
    return (Jump expr1 amount')

inc :: Parser Instruction
inc = do
    string "inc "
    reg <- rf
    return (Inc reg)

dec :: Parser Instruction
dec = do
    string "dec "
    reg <- rf
    return (Dec reg)

parseAdvent :: String -> Maybe Instruction
parseAdvent = parseMaybe (cpy <|> jnz <|> inc <|> dec)