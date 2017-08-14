module Machine.Memory (dff
                      ,bit
                      ,register
                      ,RAM8
                      ,ram8) where

import qualified Control.Monad.State as State
import qualified Machine.Gates       as Gates

-- a digital flip flop stores an input and outputs that input on the next clock cycle
-- out = input (t - 1) where t is the clock cycle count
dff :: Bool -> State.State Bool Bool
dff n = State.state (\s -> (s, n))

-- a bit register uses the dff to either store a new value or store once more the
-- previously stored value.
-- it always outputs what was stored in the dff previously
-- if load out = input (t - 1) else if not load out = out (t - 1)
bit :: Bool -> Bool -> State.State Bool Bool
bit input load = do
    currentState <- State.get
    dff $ Gates.mux currentState input load

-- a register is basically a bit that takes arrays of bits and stores arrays of bits
-- instead of just single bits
register :: [Bool] -> Bool -> State.State [Bool] [Bool]
register inputs load = do
    currentState <- State.get
    let runBit   = \input -> bit input load
    let f        = \(input, s) -> State.runState (runBit input) s
    let result   = unzip $ fmap f $ zip inputs currentState
    State.state (\_ -> result)

type RAM8 = ([Bool], [Bool], [Bool], [Bool], [Bool], [Bool], [Bool], [Bool])

runRegister :: [Bool] -> Bool -> State.State [Bool] [Bool]
runRegister = \input load -> register input load

-- This function gets 8 arrays of bools and determines based on the address
-- on which to run the load on.
-- The input and load is applied to a register with the state pulled out of the
-- 8 wide tuple from the mux8WayN logic. Then case expression logic is used from
-- the result of the dmux8Way to figure out how to put the state back together
-- again. Note the \_ arg in the state arg function. The result of the register is
-- supplied to `n`, the new state, and the resulting function ignores the previous
-- state in the state monad. It can do that because we already accounted for that
-- state with the `<- State.get call.
ram8 :: [Bool] -> Bool -> (Bool, Bool, Bool) -> State.State RAM8 [Bool]
ram8 input load address = do
    (a, b, c, d, e, f, g, h) <- State.get
    let currentState = Gates.mux8WayN a b c d e f g h address
    let (o, s)       = State.runState (runRegister input load) currentState
    let r            = Gates.dmux8Way True address
    let n = case r of (True, False, False, False, False, False, False, False) -> (o, (s, b, c, d, e, f, g, h))
                      (False, True, False, False, False, False, False, False) -> (o, (a, s, c, d, e, f, g, h))
                      (False, False, True, False, False, False, False, False) -> (o, (a, b, s, d, e, f, g, h))
                      (False, False, False, True, False, False, False, False) -> (o, (a, b, c, s, e, f, g, h))
                      (False, False, False, False, True, False, False, False) -> (o, (a, b, c, d, s, f, g, h))
                      (False, False, False, False, False, True, False, False) -> (o, (a, b, c, d, e, s, g, h))
                      (False, False, False, False, False, False, True, False) -> (o, (a, b, c, d, e, f, s, h))
                      (False, False, False, False, False, False, False, True) -> (o, (a, b, c, d, e, f, g, s))
    State.state (\_ -> n)
