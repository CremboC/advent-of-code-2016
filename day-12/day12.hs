import Day12Parse
import Data.Maybe
import Debug.Trace
import qualified Data.Map.Strict as M

type Pointer = Int
type State = (M.Map Name Int, Pointer)

cpy :: State -> Expr -> Expr -> State
cpy (state, p) (Literal a) (Register target) = (M.alter f target state, succ p)
    where f _ = Just a
cpy (state, p) (Register source) (Register target) = (M.alter f target state, succ p)
    where f _ = Just $ M.findWithDefault 0 source state
cpy _ _ _ = error "Illegal op"

jmp :: State -> Expr -> Int -> State
jmp (state, p) e am = if v /= 0 then (state, p + am) else (state, succ p)
    where v = case e of
            (Register r) -> M.findWithDefault 0 r state
            (Literal a) -> a

exec :: Instruction -> State -> State
exec (Copy e1 e2) state = cpy state e1 e2
exec (Jump e1 am) state = jmp state e1 am
exec (Inc (Register r)) (mem, p) = (M.alter inc r mem, succ p)
    where inc Nothing = Nothing; inc (Just a) = Just (succ a)
exec (Dec (Register r)) (mem, p) = (M.alter dec r mem, succ p)
    where dec Nothing = Nothing; dec (Just a) = Just (pred a)

run :: [Instruction] -> State -> State
run is state@(_, p) | p >= length is = state
run is state@(_, p) = run is $ exec (is !! p) state

main = do
    instructions <- catMaybes . map parseAdvent . lines <$> readFile "input.txt"
    -- part 1
    print $ run instructions (M.empty, 0)
    -- part 2
    print $ run instructions (M.fromList [('c', 1)], 0)