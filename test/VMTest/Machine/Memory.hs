module VMTest.Machine.Memory (memoryTests) where

import qualified Control.Monad.State as State
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

memoryTests :: [HUnit.Test]
memoryTests = [dffZeroOutOneInTest
              ,dffOneOutOneInTest
              ,dffOneOutZeroInTest
              ,dffZeroOutZeroInTest]
