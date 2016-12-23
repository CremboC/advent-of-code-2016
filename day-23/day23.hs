module Main where
    
import Day23Parse
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S

type Pointer = Int
type Registers = M.Map Name Int
type State = (Registers, Pointer)

eval :: Registers -> Expr -> Int
eval state e = case e of
            (Register r) -> M.findWithDefault 0 r state
            (Literal a) -> a

cpy :: State -> Expr -> Expr -> State
cpy (state, p) (Literal a) (Register target) = (M.alter f target state, succ p)
    where f _ = Just a
cpy (state, p) (Register source) (Register target) = (M.alter f target state, succ p)
    where f _ = Just $ M.findWithDefault 0 source state
cpy _ _ _ = error "Illegal op"

jnz :: State -> Expr -> Expr -> State
jnz (rs, p) e1 e2 = if v /= 0 then (rs, p + am) else (rs, succ p)
    where (v, am) = (eval rs e1, eval rs e2)

tgl :: State -> S.Seq Instruction -> Expr -> (State, S.Seq Instruction)
tgl (rs, p) is e = ((rs, succ p), is')
    where
        v = eval rs e
        is' = S.adjust' tgl' (p + v) is
        tgl' (Inc e) = (Dec e)
        tgl' (Dec e) = (Inc e)
        tgl' (Tgl e) = (Inc e)
        tgl' (Jnz e e2) = (Cpy e e2)
        tgl' (Cpy e1 e2) = (Jnz e1 e2)

exec :: Instruction -> State -> S.Seq Instruction -> (State, S.Seq Instruction)
exec (Cpy e1 e2) state is = (cpy state e1 e2, is)
exec (Jnz e1 e2) state is = (jnz state e1 e2, is)
exec (Inc (Register r)) (mem, p) is = ((M.alter inc r mem, succ p), is)
    where inc a = succ <$> a
exec (Dec (Register r)) (mem, p) is = ((M.alter dec r mem, succ p), is)
    where dec a = pred <$> a
exec (Tgl e1) state is = tgl state is e1

isValid :: Instruction -> Bool
isValid (Cpy _ (Literal _)) = False
isValid (Inc (Literal _))   = False
isValid (Dec (Literal _))   = False
isValid _ = True

run :: S.Seq Instruction -> State -> State
run is state@(_, p) | p >= length is = state
run is state@(rs, p) = if isValid instr then run is' state' else run is (rs, succ p)
    where 
        instr = (is `S.index` p)
        (state', is') = exec instr state is

main :: IO ()
main = do
    instructions <- S.fromList . catMaybes . map parseAdvent . lines <$> readFile "input.txt"
    -- part 1
    print $ run instructions (M.fromList [('a', 7)], 0)
    -- part 2
    -- print $ run instructions (M.fromList [('a', 12)], 0)