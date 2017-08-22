module Machine.Memory (dff
                      ,bit
                      ,register
                      ,RAM8
                      ,ram8
                      ,RAM64
                      ,ram64
                      ,Addr8) where

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
type Addr8 = (Bool, Bool, Bool)

runRegister :: [Bool] -> Bool -> State.State [Bool] [Bool]
runRegister = \input load -> register input load

-- This takes 8 arguments (either, [Bool], RAM8, RAM64, etc) and an (output, state) object
-- as well as an 8 bit address. The dmux result is a tuple of 8 bools and is used to return a new state.
-- The 8 tuple of bools informs which of the 8 states to change.
stateFromAddress a b c d e f g h (o, s) address = let
    r = Gates.dmux8Way True address
    n = case r of (True, False, False, False, False, False, False, False) -> (o, (s, b, c, d, e, f, g, h))
                  (False, True, False, False, False, False, False, False) -> (o, (a, s, c, d, e, f, g, h))
                  (False, False, True, False, False, False, False, False) -> (o, (a, b, s, d, e, f, g, h))
                  (False, False, False, True, False, False, False, False) -> (o, (a, b, c, s, e, f, g, h))
                  (False, False, False, False, True, False, False, False) -> (o, (a, b, c, d, s, f, g, h))
                  (False, False, False, False, False, True, False, False) -> (o, (a, b, c, d, e, s, g, h))
                  (False, False, False, False, False, False, True, False) -> (o, (a, b, c, d, e, f, s, h))
                  (False, False, False, False, False, False, False, True) -> (o, (a, b, c, d, e, f, g, s))
    in (n)

-- This function demultiplexes an 8 bit (three tuple bool) address to figure out
-- which state to run a function on
functionFromAddress a b c d e f g h fn address = let
    r = Gates.dmux8Way True address
    result = case r of (True, False, False, False, False, False, False, False) -> fn a
                       (False, True, False, False, False, False, False, False) -> fn b
                       (False, False, True, False, False, False, False, False) -> fn c
                       (False, False, False, True, False, False, False, False) -> fn d
                       (False, False, False, False, True, False, False, False) -> fn e
                       (False, False, False, False, False, True, False, False) -> fn f
                       (False, False, False, False, False, False, True, False) -> fn g
                       (False, False, False, False, False, False, False, True) -> fn h
    in (result)

stateFromAddress4 a b c d (o, s) address = let
    r = Gates.dmux4Way True address
    n = case r of (True, False, False, False) -> (o, (s, b, c, d))
                  (False, True, False, False) -> (o, (a, s, c, d))
                  (False, False, True, False) -> (o, (a, b, s, d))
                  (False, False, False, True) -> (o, (a, b, c, s))
    in (n)


functionFromAddress4 a b c d fn address = let
    r = Gates.dmux4Way True address
    result = case r of (True, False, False, False) -> fn a
                       (False, True, False, False) -> fn b
                       (False, False, True, False) -> fn c
                       (False, False, False, True) -> fn d
    in (result)

-- This function gets 8 arrays of bools and determines based on the address
-- on which to run the load on.
-- The input and load is applied to a register with the state pulled out of the
-- 8 wide tuple from the mux8WayN logic. Then case expression logic is used from
-- the result of the dmux8Way to figure out how to put the state back together
-- again. Note the \_ arg in the state arg function. The result of the register is
-- supplied to `n`, the new state, and the resulting function ignores the previous
-- state in the state monad. It can do that because we already accounted for that
-- state with the `<- State.get call.
ram8 :: [Bool] -> Bool -> Addr8 -> State.State RAM8 [Bool]
ram8 input load address = do
    (a, b, c, d, e, f, g, h) <- State.get
    let currentState = Gates.mux8WayN a b c d e f g h address
    let stateOutput  = State.runState (runRegister input load) currentState
    let n            = stateFromAddress a b c d e f g h stateOutput address
    State.state (\_ -> n)

type RAM64 = (RAM8, RAM8, RAM8, RAM8, RAM8, RAM8, RAM8, RAM8)

runRam8 :: [Bool] -> Bool -> Addr8 -> State.State RAM8 [Bool]
runRam8 = \input load addr -> ram8 input load addr

-- ram64 takes a bit array and a load and two 8 bit addresses and runs it
-- on a state of RAM64 (an 8 tuple of RAM8s).
ram64 :: [Bool] -> Bool -> (Addr8, Addr8) -> State.State RAM64 [Bool]
ram64 input load (addr64, addr8) = do
    (a, b, c, d, e, f, g, h) <- State.get
    let dmuxResult = Gates.dmux8Way True addr64
    let fn = State.runState (runRam8 input load addr8)
    let stateOutput = functionFromAddress a b c d e f g h fn addr64
    let n = stateFromAddress a b c d e f g h stateOutput addr64
    State.state (\_ -> n)

type RAM512 = (RAM64, RAM64, RAM64, RAM64, RAM64, RAM64, RAM64, RAM64)
runRam64 :: [Bool] -> Bool -> (Addr8, Addr8) -> State.State RAM64 [Bool]
runRam64 = \input load addr -> ram64 input load addr

ram512 :: [Bool] -> Bool -> (Addr8, Addr8, Addr8) -> State.State RAM512 [Bool]
ram512 input load (addr512, addr64, addr8) = do
    (a, b, c, d, e, f, g, h) <- State.get
    let dmuxResult = Gates.dmux8Way True addr512
    let fn = State.runState (runRam64 input load (addr64, addr8))
    let stateOutput = functionFromAddress a b c d e f g h fn addr512
    let n = stateFromAddress a b c d e f g h stateOutput addr512
    State.state (\_ -> n)

type RAM4K = (RAM512, RAM512, RAM512, RAM512, RAM512, RAM512, RAM512, RAM512)
runRam512 :: [Bool] -> Bool -> (Addr8, Addr8, Addr8) -> State.State RAM512 [Bool]
runRam512 = \input load addr -> ram512 input load addr

ram4K :: [Bool] -> Bool -> (Addr8, Addr8, Addr8, Addr8) -> State.State RAM4K [Bool]
ram4K input load (addr4k, addr512, addr64, addr8) = do
    (a, b, c, d, e, f, g, h) <- State.get
    let dmuxResult = Gates.dmux8Way True addr4k
    let fn = State.runState (runRam512 input load (addr512, addr64, addr8))
    let stateOutput = functionFromAddress a b c d e f g h fn addr4k
    let n = stateFromAddress a b c d e f g h stateOutput addr4k
    State.state (\_ -> n)

type Addr4 = (Bool, Bool)
type RAM16K = (RAM4K, RAM4K, RAM4K, RAM4K)
runRam4K :: [Bool] -> Bool -> (Addr8, Addr8, Addr8, Addr8) -> State.State RAM4K [Bool]
runRam4K = \input load addr -> ram4K input load addr

ram16K :: [Bool] -> Bool -> (Addr4, Addr8, Addr8, Addr8, Addr8) -> State.State RAM16K [Bool]
ram16K input load (addr16, addr4k, addr512, addr64, addr8) = do
    (a, b, c, d) <- State.get
    let dmuxResult = Gates.dmux4Way True addr16
    let fn = State.runState (runRam4K input load (addr4k, addr512, addr64, addr8))
    let stateOutput = functionFromAddress4 a b c d fn addr16
    let n = stateFromAddress4 a b c d stateOutput addr16
    State.state (\_ -> n)
