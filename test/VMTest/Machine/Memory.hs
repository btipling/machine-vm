module VMTest.Machine.Memory (memoryTests) where

import qualified Control.Monad.State as State
import qualified Data.List           as List
import qualified Machine.Memory      as Memory
import qualified Test.HUnit          as HUnit

runDff :: Bool -> State.State Bool Bool
runDff b = do
    Memory.dff b

dffZeroOutOneInTest :: HUnit.Test
dffZeroOutOneInTest = let
    dffStart = toEnum 0
    dffIn    = toEnum 1
    expected = (toEnum 0, toEnum 1)
    out      = State.runState (runDff dffIn) dffStart
    in (HUnit.TestCase $ HUnit.assertEqual "dff 0 1 should validate" expected out)

dffOneOutOneInTest :: HUnit.Test
dffOneOutOneInTest = let
    dffStart = toEnum 1
    dffIn    = toEnum 1
    expected = (toEnum 1, toEnum 1)
    out      = State.runState (runDff dffIn) dffStart
    in (HUnit.TestCase $ HUnit.assertEqual "dff 1 1 should validate" expected out)

dffOneOutZeroInTest :: HUnit.Test
dffOneOutZeroInTest = let
    dffStart = toEnum 1
    dffIn    = toEnum 0
    expected = (toEnum 1, toEnum 0)
    out      = State.runState (runDff dffIn) dffStart
    in (HUnit.TestCase $ HUnit.assertEqual "dff 1 0 should validate" expected out)

dffZeroOutZeroInTest :: HUnit.Test
dffZeroOutZeroInTest = let
    dffStart = toEnum 0
    dffIn    = toEnum 0
    expected = (toEnum 0, toEnum 0)
    out      = State.runState (runDff dffIn) dffStart
    in (HUnit.TestCase $ HUnit.assertEqual "dff 0 0 should validate" expected out)

runBit :: Bool -> Bool -> State.State Bool Bool
runBit input load = do
    Memory.bit input load

testBit :: HUnit.Test
testBit = let
    inputs = [(1, 1, 1, (toEnum 1, toEnum 1))
             ,(1, 0, 1, (toEnum 1, toEnum 1))
             ,(1, 1, 0, (toEnum 0, toEnum 1))  -- when load is 1 input is is stored and the previous state is the output
             ,(1, 0, 0, (toEnum 0, toEnum 0))  -- when load is 0 input is ignored and output is stored, output is the previous state
             ,(0, 0, 0, (toEnum 0, toEnum 0))
             ,(0, 1, 0, (toEnum 0, toEnum 0))
             ,(0, 0, 1, (toEnum 1, toEnum 1))  -- when load is 0 out is 1 and stored is 1 as we're storing what we had previously
             ,(0, 1, 1, (toEnum 1, toEnum 0))] -- when load is 1 out is 1 and stored is 0 as we're storing the new input
    fn     = \(input, load, stateStart, expected) -> (State.runState (runBit (toEnum input) (toEnum load)) (toEnum stateStart)) /= expected
    result = List.find fn inputs
    in (HUnit.TestCase $ HUnit.assertEqual "bit should validate" Nothing result)

runRegister :: [Bool] -> Bool -> State.State [Bool] [Bool]
runRegister inputs load = do
    Memory.register inputs load

i2b :: [Int] -> [Bool]
i2b ints = fmap toEnum ints

-- the following checks are pretty much the same as above for testBit but for lists
testRegister :: HUnit.Test
testRegister = let
    inputs = [((i2b [1, 1, 1, 1, 1, 1, 1, 1]), toEnum 1, (i2b [1, 1, 1, 1, 1, 1, 1, 1]), ((i2b [1, 1, 1, 1, 1, 1, 1, 1]), (i2b [1, 1, 1, 1, 1, 1, 1, 1])))
             ,((i2b [1, 1, 1, 1, 1, 1, 1, 1]), toEnum 0, (i2b [1, 1, 1, 1, 1, 1, 1, 1]), ((i2b [1, 1, 1, 1, 1, 1, 1, 1]), (i2b [1, 1, 1, 1, 1, 1, 1, 1])))
             ,((i2b [1, 1, 1, 1, 1, 1, 1, 1]), toEnum 1, (i2b [0, 0, 0, 0, 0, 0, 0, 0]), ((i2b [0, 0, 0, 0, 0, 0, 0, 0]), (i2b [1, 1, 1, 1, 1, 1, 1, 1])))
             ,((i2b [1, 1, 1, 1, 1, 1, 1, 1]), toEnum 0, (i2b [0, 0, 0, 0, 0, 0, 0, 0]), ((i2b [0, 0, 0, 0, 0, 0, 0, 0]), (i2b [0, 0, 0, 0, 0, 0, 0, 0])))
             ,((i2b [0, 0, 0, 0, 0, 0, 0, 0]), toEnum 0, (i2b [0, 0, 0, 0, 0, 0, 0, 0]), ((i2b [0, 0, 0, 0, 0, 0, 0, 0]), (i2b [0, 0, 0, 0, 0, 0, 0, 0])))
             ,((i2b [0, 0, 0, 0, 0, 0, 0, 0]), toEnum 1, (i2b [0, 0, 0, 0, 0, 0, 0, 0]), ((i2b [0, 0, 0, 0, 0, 0, 0, 0]), (i2b [0, 0, 0, 0, 0, 0, 0, 0])))
             ,((i2b [0, 0, 0, 0, 0, 0, 0, 0]), toEnum 0, (i2b [1, 1, 1, 1, 1, 1, 1, 1]), ((i2b [1, 1, 1, 1, 1, 1, 1, 1]), (i2b [1, 1, 1, 1, 1, 1, 1, 1])))
             ,((i2b [0, 0, 0, 0, 0, 0, 0, 0]), toEnum 1, (i2b [1, 1, 1, 1, 1, 1, 1, 1]), ((i2b [1, 1, 1, 1, 1, 1, 1, 1]), (i2b [0, 0, 0, 0, 0, 0, 0, 0])))
             ,((i2b [0, 0, 0, 0, 1, 1, 1, 1]), toEnum 0, (i2b [1, 1, 1, 1, 0, 0, 0, 0]), ((i2b [1, 1, 1, 1, 0, 0, 0, 0]), (i2b [1, 1, 1, 1, 0, 0, 0, 0])))
             ,((i2b [0, 0, 0, 0, 1, 1, 1, 1]), toEnum 1, (i2b [1, 1, 1, 1, 0, 0, 0, 0]), ((i2b [1, 1, 1, 1, 0, 0, 0, 0]), (i2b [0, 0, 0, 0, 1, 1, 1, 1])))]
    fn     = \(inputs, load, stateStart, expected) -> (State.runState (runRegister inputs load) stateStart) /= expected
    result = List.find fn inputs
    in (HUnit.TestCase $ HUnit.assertEqual "register should validate" Nothing result)

memoryTests :: [HUnit.Test]
memoryTests = [dffZeroOutOneInTest
              ,dffOneOutOneInTest
              ,dffOneOutZeroInTest
              ,dffZeroOutZeroInTest
              ,testBit
              ,testRegister]
