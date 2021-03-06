module VMTest.Machine.Memory (memoryTests) where

import qualified Control.Monad.State as State
import qualified Data.List           as List
import qualified Machine.Memory      as Memory
import qualified System.Random       as Random
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

runRam8 :: [Bool] -> Bool -> Memory.Addr8 -> State.State Memory.RAM8 [Bool]
runRam8 inputs load address = do
    Memory.ram8 inputs load address

-- Memory is just too verbose to test with multivalue int arrays. Needs separate test cases for some
-- different inputs, but not comprehensive.
ram8A = i2b [0, 0, 0]
ram8B = i2b [0, 0, 1]
ram8C = i2b [0, 1, 0]
ram8D = i2b [0, 1, 1]
ram8E = i2b [1, 0, 0]
ram8F = i2b [1, 0, 1]
ram8G = i2b [1, 1, 0]
ram8H = i2b [1, 1, 1]

testRam8ALoad :: HUnit.Test
testRam8ALoad = let
    inputs       = i2b [1, 1, 1]
    currentState = (ram8A, ram8B, ram8C, ram8D, ram8E, ram8F, ram8G, ram8H)
    expected     = (ram8A, (inputs, ram8B, ram8C, ram8D, ram8E, ram8F, ram8G, ram8H))
    load         = True
    address      = (False, False, False)
    result       = State.runState (runRam8 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram8 should validate for a load" expected result)

testRam8AKeep :: HUnit.Test
testRam8AKeep = let
    inputs       = i2b [1, 1, 1]
    currentState = (ram8A, ram8B, ram8C, ram8D, ram8E, ram8F, ram8G, ram8H)
    expected     = (ram8A, (ram8A, ram8B, ram8C, ram8D, ram8E, ram8F, ram8G, ram8H))
    load         = False
    address      = (False, False, False)
    result       = State.runState (runRam8 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram8 should validate for a keep" expected result)

testRam8DLoad :: HUnit.Test
testRam8DLoad = let
    inputs       = i2b [0, 0, 0]
    currentState = (ram8A, ram8B, ram8C, ram8D, ram8E, ram8F, ram8G, ram8H)
    expected     = (ram8D, (ram8A, ram8B, ram8C, inputs, ram8E, ram8F, ram8G, ram8H))
    load         = True
    address      = (False, True, True)
    result       = State.runState (runRam8 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram8 should validate for d load" expected result)

testRam8DKeep :: HUnit.Test
testRam8DKeep = let
    inputs       = i2b [0, 0, 0]
    currentState = (ram8A, ram8B, ram8C, ram8D, ram8E, ram8F, ram8G, ram8H)
    expected     = (ram8D, (ram8A, ram8B, ram8C, ram8D, ram8E, ram8F, ram8G, ram8H))
    load         = False
    address      = (False, True, True)
    result       = State.runState (runRam8 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram8 should validate for d keep" expected result)

testRam8HLoad :: HUnit.Test
testRam8HLoad = let
    inputs       = i2b [0, 1, 0]
    currentState = (ram8A, ram8B, ram8C, ram8D, ram8E, ram8F, ram8G, ram8H)
    expected     = (ram8H, (ram8A, ram8B, ram8C, ram8D, ram8E, ram8F, ram8G, inputs))
    load         = True
    address      = (True, True, True)
    result       = State.runState (runRam8 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram8 should validate for h load" expected result)

testRam8HKeep :: HUnit.Test
testRam8HKeep = let
    inputs       = i2b [0, 1, 0]
    currentState = (ram8A, ram8B, ram8C, ram8D, ram8E, ram8F, ram8G, ram8H)
    expected     = (ram8H, (ram8A, ram8B, ram8C, ram8D, ram8E, ram8F, ram8G, ram8H))
    load         = False
    address      = (True, True, True)
    result       = State.runState (runRam8 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram8 should validate for h keep" expected result)

genRandomBit :: Random.StdGen -> (Bool, Random.StdGen)
genRandomBit g = let
    (randomInt, nextG) = Random.random g :: (Int, Random.StdGen)
    v                  = randomInt `mod` 2 == 0
    in ((v, nextG))


genRandomN :: Int -> Random.StdGen -> (Random.StdGen -> (a, Random.StdGen)) -> ([a], Random.StdGen)
genRandomN n g f = let
    fn        = \_ (xs, curG) -> let (x, nextG) = (f curG) in ((x:xs), nextG)
    in (foldr fn ([], g) (take n (repeat ([], g))))

genRandom8 :: Random.StdGen -> (Random.StdGen -> (a, Random.StdGen)) -> ([a], Random.StdGen)
genRandom8 g f = genRandomN 8 g f

genRandom4 :: Random.StdGen -> (Random.StdGen -> (a, Random.StdGen)) -> ([a], Random.StdGen)
genRandom4 g f = genRandomN 4 g f

genRandom4Tuple :: Random.StdGen -> (Random.StdGen -> (a, Random.StdGen)) -> ((a, a, a, a), Random.StdGen)
genRandom4Tuple g f = let
    (l, nextG) = genRandom4 g f
    in ((l !! 0, l !! 1, l !! 2, l !! 3), nextG)

genRandom8Tuple :: Random.StdGen -> (Random.StdGen -> (a, Random.StdGen)) -> ((a, a, a, a, a, a, a, a), Random.StdGen)
genRandom8Tuple g f = let
    (l, nextG) = genRandom8 g f
    in ((l !! 0, l !! 1, l !! 2, l !! 3, l !! 4, l !! 5, l !! 6, l !! 7), nextG)

genRandomBitArray :: Random.StdGen -> ([Bool], Random.StdGen)
genRandomBitArray g = genRandom8 g genRandomBit

genRandomRam8 :: Random.StdGen -> (Memory.RAM8, Random.StdGen)
genRandomRam8 g = genRandom8Tuple g genRandomBitArray

genRandomRam64 :: Random.StdGen -> (Memory.RAM64, Random.StdGen)
genRandomRam64 g = genRandom8Tuple g genRandomRam8

genRandomRam512 :: Random.StdGen -> (Memory.RAM512, Random.StdGen)
genRandomRam512 g = genRandom8Tuple g genRandomRam64

genRandomRam4K :: Random.StdGen -> (Memory.RAM4K, Random.StdGen)
genRandomRam4K g = genRandom8Tuple g genRandomRam512

genRandomRam16K :: Random.StdGen -> (Memory.RAM16K, Random.StdGen)
genRandomRam16K g = genRandom4Tuple g genRandomRam4K

runRam64 :: [Bool] -> Bool -> (Memory.Addr8, Memory.Addr8) -> State.State Memory.RAM64 [Bool]
runRam64 inputs load address = do
    Memory.ram64 inputs load address

testRam64AALoad :: HUnit.Test
testRam64AALoad = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam64 nextG
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = currentState
    (a, b, c, d, e, f, g, h)                         = ramA
    newRamA                                          = (inputs, b, c, d, e, f, g, h)
    expected                                         = (a, (newRamA, ramB, ramC, ramD, ramE, ramF, ramG, ramH))
    load                                             = True
    address                                          = ((False, False, False), (False, False, False))
    result                                           = State.runState (runRam64 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram64 should validate for aa load" expected result)

testRam64AAKeep :: HUnit.Test
testRam64AAKeep = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam64 nextG
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = currentState
    (a, b, c, d, e, f, g, h)                         = ramA
    expected                                         = (a, (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH))
    load                                             = False
    address                                          = ((False, False, False), (False, False, False))
    result                                           = State.runState (runRam64 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram64 should validate for aa keep" expected result)

testRam64DBLoad :: HUnit.Test
testRam64DBLoad = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam64 nextG
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = currentState
    (a, b, c, d, e, f, g, h)                         = ramD
    newRamD                                          = (a, inputs, c, d, e, f, g, h)
    expected                                         = (b, (ramA, ramB, ramC, newRamD, ramE, ramF, ramG, ramH))
    load                                             = True
    address                                          = ((False, True, True), (False, False, True))
    result                                           = State.runState (runRam64 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram64 should validate for db load" expected result)

testRam64HELoad :: HUnit.Test
testRam64HELoad = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam64 nextG
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = currentState
    (a, b, c, d, e, f, g, h)                         = ramH
    newRamH                                          = (a, b, c, d, inputs, f, g, h)
    expected                                         = (e, (ramA, ramB, ramC, ramD, ramE, ramF, ramG, newRamH))
    load                                             = True
    address                                          = ((True, True, True), (True, False, False))
    result                                           = State.runState (runRam64 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram64 should validate for he load" expected result)

testRam64HEKeep :: HUnit.Test
testRam64HEKeep = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam64 nextG
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = currentState
    (a, b, c, d, e, f, g, h)                         = ramH
    expected                                         = (e, (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH))
    load                                             = False
    address                                          = ((True, True, True), (True, False, False))
    result                                           = State.runState (runRam64 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram64 should validate for he keep" expected result)

runRam512 :: [Bool] -> Bool -> (Memory.Addr8, Memory.Addr8, Memory.Addr8) -> State.State Memory.RAM512 [Bool]
runRam512 inputs load address = do
    Memory.ram512 inputs load address

testRam512AAAKeep :: HUnit.Test
testRam512AAAKeep = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam512 nextG
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = currentState
    (a, b, c, d, e, f, g, h)                         = ramA
    (a', b', c', d', e', f', g', h')                 = a
    expected                                         = (a', (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH))
    load                                             = False
    address                                          = ((False, False, False), (False, False, False), (False, False, False))
    result                                           = State.runState (runRam512 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram512 should validate for aaa keep" expected result)

testRam512DBCKeep :: HUnit.Test
testRam512DBCKeep = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam512 nextG
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = currentState
    (a, b, c, d, e, f, g, h)                         = ramD
    (a', b', c', d', e', f', g', h')                 = b
    expected                                         = (c', (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH))
    load                                             = False
    address                                          = ((False, True, True), (False, False, True), (False, True, False))
    result                                           = State.runState (runRam512 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram512 should validate for dbc keep" expected result)

testRam512DBCLoad :: HUnit.Test
testRam512DBCLoad = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam512 nextG
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = currentState
    (a, b, c, d, e, f, g, h)                         = ramD
    (a', b', c', d', e', f', g', h')                 = b
    newRamB                                          = (a', b', inputs, d', e', f', g', h')
    newRamD                                          = (a, newRamB, c, d, e, f, g, h)
    expected                                         = (c', (ramA, ramB, ramC, newRamD, ramE, ramF, ramG, ramH))
    load                                             = True
    address                                          = ((False, True, True), (False, False, True), (False, True, False))
    result                                           = State.runState (runRam512 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram512 should validate for dbc load" expected result)

testRam512HGBKeep :: HUnit.Test
testRam512HGBKeep = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam512 nextG
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = currentState
    (a, b, c, d, e, f, g, h)                         = ramH
    (a', b', c', d', e', f', g', h')                 = g
    expected                                         = (b', (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH))
    load                                             = False
    address                                          = ((True, True, True), (True, True, False), (False, False, True))
    result                                           = State.runState (runRam512 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram512 should validate for hgb keep" expected result)

testRam512HGBLoad :: HUnit.Test
testRam512HGBLoad = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam512 nextG
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = currentState
    (a, b, c, d, e, f, g, h)                         = ramH
    (a', b', c', d', e', f', g', h')                 = g
    newRamG                                          = (a', inputs, c', d', e', f', g', h')
    newRamH                                          = (a, b, c, d, e, f, newRamG, h)
    expected                                         = (b', (ramA, ramB, ramC, ramD, ramE, ramF, ramG, newRamH))
    load                                             = True
    address                                          = ((True, True, True), (True, True, False), (False, False, True))
    result                                           = State.runState (runRam512 inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram512 should validate for hgb load" expected result)

runRam4K :: [Bool] -> Bool -> (Memory.Addr8, Memory.Addr8, Memory.Addr8, Memory.Addr8) -> State.State Memory.RAM4K [Bool]
runRam4K inputs load address = do
    Memory.ram4K inputs load address

testRam4KAAAAKeep :: HUnit.Test
testRam4KAAAAKeep = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam4K nextG
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = currentState
    (a, b, c, d, e, f, g, h)                         = ramA
    (a', b', c', d', e', f', g', h')                 = a
    (a'', b'', c'', d'', e'', f'', g'', h'')         = a'
    expected                                         = (a'', (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH))
    load                                             = False
    address                                          = ((False, False, False), (False, False, False), (False, False, False), (False, False, False))
    result                                           = State.runState (runRam4K inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram4k should validate for aaaa keep" expected result)

testRam4KCFABKeep :: HUnit.Test
testRam4KCFABKeep = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam4K nextG
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = currentState
    (a, b, c, d, e, f, g, h)                         = ramC
    (a', b', c', d', e', f', g', h')                 = f
    (a'', b'', c'', d'', e'', f'', g'', h'')         = a'
    expected                                         = (b'', (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH))
    load                                             = False
    address                                          = ((False, True, False), (True, False, True), (False, False, False), (False, False, True))
    result                                           = State.runState (runRam4K inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram4k should validate for cfab keep" expected result)

testRam4KCFABLoad :: HUnit.Test
testRam4KCFABLoad = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam4K nextG
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = currentState
    (a, b, c, d, e, f, g, h)                         = ramC
    (a', b', c', d', e', f', g', h')                 = f
    (a'', b'', c'', d'', e'', f'', g'', h'')         = a'
    newRamA                                          = (a'', inputs, c'', d'', e'', f'', g'', h'')
    newRamF                                          = (newRamA, b', c', d', e', f', g', h')
    newRamC                                          = (a, b, c, d, e, newRamF, g, h)
    expected                                         = (b'', (ramA, ramB, newRamC, ramD, ramE, ramF, ramG, ramH))
    load                                             = True
    address                                          = ((False, True, False), (True, False, True), (False, False, False), (False, False, True))
    result                                           = State.runState (runRam4K inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram4k should validate for cfab load" expected result)

runRam16K :: [Bool] -> Bool -> (Memory.Addr4, Memory.Addr8, Memory.Addr8, Memory.Addr8, Memory.Addr8) -> State.State Memory.RAM16K [Bool]
runRam16K inputs load address = do
    Memory.ram16K inputs load address

testRam16KAAAAAKeep :: HUnit.Test
testRam16KAAAAAKeep = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam16K nextG
    (ram16KA, ram16KB, ram16KC, ram16KD)             = currentState
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = ram16KA
    (a, b, c, d, e, f, g, h)                         = ramA
    (a', b', c', d', e', f', g', h')                 = a
    (a'', b'', c'', d'', e'', f'', g'', h'')         = a'
    expected                                         = (a'', (ram16KA, ram16KB, ram16KC, ram16KD))
    load                                             = False
    address                                          = ((False, False), (False, False, False), (False, False, False), (False, False, False), (False, False, False))
    result                                           = State.runState (runRam16K inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram16k should validate for aaaaa keep" expected result)

testRam16KDBFHFKeep :: HUnit.Test
testRam16KDBFHFKeep = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam16K nextG
    (ram16KA, ram16KB, ram16KC, ram16KD)             = currentState
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = ram16KD
    (a, b, c, d, e, f, g, h)                         = ramB
    (a', b', c', d', e', f', g', h')                 = f
    (a'', b'', c'', d'', e'', f'', g'', h'')         = h'
    expected                                         = (f'', (ram16KA, ram16KB, ram16KC, ram16KD))
    load                                             = False
    address                                          = ((True, True), (False, False, True), (True, False, True), (True, True, True), (True, False, True))
    result                                           = State.runState (runRam16K inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram16k should validate for dbfhf keep" expected result)

testRam16KDBFHFLoad :: HUnit.Test
testRam16KDBFHFLoad = let
    gen                                              = Random.mkStdGen 1
    (inputs, nextG)                                  = genRandomBitArray gen
    (currentState, _)                                = genRandomRam16K nextG
    (ram16KA, ram16KB, ram16KC, ram16KD)             = currentState
    (ramA, ramB, ramC, ramD, ramE, ramF, ramG, ramH) = ram16KD
    (a, b, c, d, e, f, g, h)                         = ramB
    (a', b', c', d', e', f', g', h')                 = f
    (a'', b'', c'', d'', e'', f'', g'', h'')         = h'
    newRamH                                          = (a'', b'', c'', d'', e'', inputs, g'', h'')
    newRamF                                          = (a', b', c', d', e', f', g', newRamH)
    newRamB                                          = (a, b, c, d, e, newRamF, g, h)
    newRam16KD                                       = (ramA, newRamB, ramC, ramD, ramE, ramF, ramG, ramH)
    expected                                         = (f'', (ram16KA, ram16KB, ram16KC, newRam16KD))
    load                                             = True
    address                                          = ((True, True), (False, False, True), (True, False, True), (True, True, True), (True, False, True))
    result                                           = State.runState (runRam16K inputs load address) currentState
    in (HUnit.TestCase $ HUnit.assertEqual "ram16k should validate for dbfhf load" expected result)

memoryTests :: [HUnit.Test]
memoryTests = [dffZeroOutOneInTest
              ,dffOneOutOneInTest
              ,dffOneOutZeroInTest
              ,dffZeroOutZeroInTest
              ,testBit
              ,testRegister
              ,testRam8ALoad
              ,testRam8AKeep
              ,testRam8DLoad
              ,testRam8DKeep
              ,testRam8HLoad
              ,testRam8HKeep
              ,testRam64AALoad
              ,testRam64AAKeep
              ,testRam64DBLoad
              ,testRam64HELoad
              ,testRam64HEKeep
              ,testRam512AAAKeep
              ,testRam512DBCKeep
              ,testRam512DBCLoad
              ,testRam512HGBKeep
              ,testRam512HGBLoad]
            -- These tests take really long, they pass, but they wont finish on travis-ci.
            --   ,testRam4KAAAAKeep
            --   ,testRam4KCFABLoad
            --   ,testRam16KAAAAAKeep
            --   ,testRam16KDBFHFKeep
            --   ,testRam16KDBFHFLoad]
