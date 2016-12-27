module Main where
    
import Day25Parse
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (find)
import qualified Data.Map as M
import qualified Data.Sequence as S
import Debug.Trace

type Pointer = Int
type Registers = M.Map Name Int
type State = (Registers, Pointer)

run :: S.Seq Instruction -> State -> [Int]
run is (_, p) | p >= length is = []
run is (mem, p) = (fromMaybe [] out) ++ (run is state')
    where
        (state', out) = exec (is `S.index` p)

        exec :: Instruction -> (State, Maybe [Int])
        exec (Cpy e1 e2) = (cpy e1 e2, Nothing)
        exec (Jnz e1 e2) = (jnz e1 e2, Nothing)
        exec (Inc (Register r)) = let inc a = succ <$> a in ((M.alter inc r mem, succ p), Nothing)
        exec (Dec (Register r)) = let dec a = pred <$> a in ((M.alter dec r mem, succ p), Nothing)
        exec (Out e1) = ((mem, succ p), Just $ [eval e1])

        jnz :: Expr -> Expr -> State
        jnz e1 e2 = if v /= 0 then (mem, p + am) else (mem, succ p)
            where (v, am) = (eval e1, eval e2)

        cpy :: Expr -> Expr -> State
        cpy (Literal val) (Register target) = (M.insert target val mem, succ p)
        cpy (Register source) (Register target) = (M.insert target val mem, succ p)
            where val = M.findWithDefault 0 source mem
        cpy _ _ = error "Illegal op"

        eval :: Expr -> Int
        eval (Register r) = M.findWithDefault 0 r mem
        eval (Literal val) = val

main :: IO ()
main = do
    instructions <- S.fromList . catMaybes . map parseAdvent . lines <$> readFile "input.txt"
    
    -- part 1
    let target = take 20 . cycle $ [0, 1]
    let mkState a = (M.fromList [('a', a)], 0)
    let run' a = (== target) . take 20 . run instructions . mkState $ a
    print $ find run' [0..]