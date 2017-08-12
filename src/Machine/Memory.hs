module Machine.Memory (dff
                      ,bit
                      ,register) where

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
    let runBit = \input -> bit input load
    let f      = \(input, s) -> State.runState (runBit input) s
    let result = unzip $ fmap f $ zip inputs currentState
    let stateify = \(inputs, newState) -> \s -> (inputs, newState)
    State.state (stateify result)
