module Day25Parse (
        parseAdvent,
        Instruction(Cpy, Jnz, Inc, Dec, Out),
        Expr(Literal, Register),
        Name
    ) where

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

type Name = Char
data Expr = Literal Int | Register Name deriving Show
data Instruction = Cpy Expr Expr | Jnz Expr Expr | Inc Expr | Dec Expr | Out Expr deriving Show

parseAdvent :: String -> Maybe Instruction
parseAdvent = parseMaybe (cpy <|> jnz <|> inc <|> dec <|> out)
    where
        cpy = Cpy <$> (string "cpy " *> choice [nf, rf]) <*> (space *> choice [nf, rf])
        jnz = Jnz <$> (string "jnz " *> choice [nf, rf]) <*> (space *> choice [nf, rf])
        inc = Inc <$> (string "inc " *> rf)
        dec = Dec <$> (string "dec " *> rf)
        out = Out <$> (string "out " *> rf)
        rf = (Register <$> letterChar) :: Parser Expr
        nf = (Literal <$> signedInt) :: Parser Expr
        signedInt = fromIntegral <$> (L.signed space L.integer)