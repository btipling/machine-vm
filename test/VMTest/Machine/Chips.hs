module VMTest.Machine.Chips (chipTests) where

import qualified Data.Bits     as Bits
import qualified Data.List     as List
import qualified Machine.Chips as Chips
import qualified Test.HUnit    as HUnit

testTwoInputGate :: String -> (Bool -> Bool -> Bool) -> [(Int, Int, Int)] -> HUnit.Test
testTwoInputGate name gate values = let
    result = List.find (\(a, b, expected) -> (gate (toEnum a) (toEnum b)) /= (toEnum expected)) values
    in (HUnit.TestCase $ HUnit.assertEqual (name ++ " should validate") Nothing result)

testThreeInputGate :: String -> (Bool -> Bool -> Bool -> Bool) -> [(Int, Int, Int, Int)] -> HUnit.Test
testThreeInputGate name gate values = let
    result = List.find (\(a, b, c, expected) -> (gate (toEnum a) (toEnum b) (toEnum c)) /= (toEnum expected)) values
    in (HUnit.TestCase $ HUnit.assertEqual (name ++ " should validate") Nothing result)

testChipsNot :: HUnit.Test
testChipsNot = let
    values = [(0, 1)
             ,(1, 0)]
    result = List.find (\(a, expected) -> (Chips.not (toEnum a)) /= (toEnum expected)) values
    in (HUnit.TestCase $ HUnit.assertEqual ("not should validate") Nothing result)

testChipsNand :: HUnit.Test
testChipsNand = testTwoInputGate "Nand" Chips.nand [(0, 0, 1)
                                                   ,(1, 0, 1)
                                                   ,(0, 1, 1)
                                                   ,(1, 1, 0)]

testChipsAnd :: HUnit.Test
testChipsAnd = testTwoInputGate "And" Chips.and [(0, 0, 0)
                                                ,(1, 0, 0)
                                                ,(0, 1, 0)
                                                ,(1, 1, 1)]

testChipsOr :: HUnit.Test
testChipsOr = testTwoInputGate "Or" Chips.or [(0, 0, 0)
                                             ,(1, 0, 1)
                                             ,(0, 1, 1)
                                             ,(1, 1, 1)]

testChipsXor :: HUnit.Test
testChipsXor = testTwoInputGate "Xor" Chips.xor [(0, 0, 0)
                                                ,(1, 0, 1)
                                                ,(0, 1, 1)
                                                ,(1, 1, 0)]

testChipsMux :: HUnit.Test
testChipsMux = testThreeInputGate "Mux" Chips.mux [(0, 0, 0, 0)
                                                  ,(0, 1, 0, 0)
                                                  ,(1, 0, 0, 1)
                                                  ,(1, 1, 0, 1)
                                                  ,(0, 0, 1, 0)
                                                  ,(0, 1, 1, 1)
                                                  ,(1, 0, 1, 0)
                                                  ,(1, 1, 1, 1)]

testChipsDMux :: HUnit.Test
testChipsDMux = let
    values    = [(0, 0, (0, 0))
                ,(0, 1, (1, 0))
                ,(1, 0, (0, 0))
                ,(1, 1, (0, 1))]
    f         = \(sel, i, (a, b)) -> (Chips.dmux (toEnum sel) (toEnum i)) /= ((toEnum a), (toEnum b))
    result    = List.find f values
    in (HUnit.TestCase $ HUnit.assertEqual ("dmux should validate") Nothing result)

testNInputGate :: String -> [Bool] -> [Bool] -> HUnit.Test
testNInputGate name output expected = let
    result = List.find (\(input, output) -> input /= output) (zip expected output)
    in (HUnit.TestCase $ HUnit.assertEqual (name ++ " should validate") Nothing result)

testNotN :: HUnit.Test
testNotN = let
    input     = fmap toEnum [  0, 1, 0, 0,    0, 1, 1, 1,    1, 0, 1, 0,    1, 1, 1, 1  ]
    expected  = fmap toEnum [  1, 0, 1, 1,    1, 0, 0, 0,    0, 1, 0, 1,    0, 0, 0, 0  ]
    output    = Chips.notN input
    in (testNInputGate "notN" output expected)

testAndN :: HUnit.Test
testAndN = let
    a         = fmap toEnum [  0, 1, 0, 0,    0, 1, 0, 1,    1, 1, 1, 1,    0, 1, 0, 1  ]
    b         = fmap toEnum [  1, 1, 0, 1,    0, 0, 0, 1,    1, 0, 1, 0,    1, 1, 0, 1  ]
    expected  = fmap toEnum [  0, 1, 0, 0,    0, 0, 0, 1,    1, 0, 1, 0,    0, 1, 0, 1  ]
    output    = Chips.andN a b
    in (testNInputGate "andN" output expected)

testOrN :: HUnit.Test
testOrN = let
    a         = fmap toEnum [  0, 1, 0, 0,    0, 1, 0, 1,    1, 1, 1, 1,    0, 1, 0, 1  ]
    b         = fmap toEnum [  1, 1, 0, 1,    0, 0, 0, 1,    1, 0, 1, 0,    1, 1, 1, 1  ]
    expected  = fmap toEnum [  1, 1, 0, 1,    0, 1, 0, 1,    1, 1, 1, 1,    1, 1, 1, 1  ]
    output    = Chips.orN a b
    in (testNInputGate "orN" output expected)

testMuxNa :: HUnit.Test
testMuxNa = let
    sel      = toEnum 0
    a        = fmap toEnum [  0, 1, 0, 0,    0, 1, 0, 1,    1, 1, 1, 1,    0, 1, 0, 1  ]
    b        = fmap toEnum [  1, 1, 0, 1,    0, 0, 0, 1,    1, 0, 1, 0,    1, 1, 1, 1  ]
    result   = Chips.muxN sel a b
    in (HUnit.TestCase $ HUnit.assertEqual ("muxN should validate for a sel") a result)

testMuxNb :: HUnit.Test
testMuxNb = let
    sel      = toEnum 1
    a        = fmap toEnum [  0, 1, 0, 0,    0, 1, 0, 1,    1, 1, 1, 1,    0, 1, 0, 1  ]
    b        = fmap toEnum [  1, 1, 0, 1,    0, 0, 0, 1,    1, 0, 1, 0,    1, 1, 1, 1  ]
    result   = Chips.muxN sel a b
    in (HUnit.TestCase $ HUnit.assertEqual ("muxN should validate for a sel") b result)

testOrNWay :: HUnit.Test
testOrNWay = let
    inputs = [([0, 1, 0, 0,    0, 1, 0, 1,    1, 1, 1, 1,    0, 1, 0, 1], 1)
             ,([0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0,    0, 1, 0, 0], 1)
             ,([0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0], 0)]
    f      = \(input, expected) -> (Chips.orNWay (fmap toEnum input)) /= toEnum expected
    result = List.find f inputs
    in (HUnit.TestCase $ HUnit.assertEqual ("orNWay should validate") Nothing result)

testMux4WayN :: HUnit.Test
testMux4WayN = let
    a         = fmap toEnum [0, 1, 0, 0,    0, 1, 0, 1,    1, 1, 1, 1,    0, 1, 0, 1]
    b         = fmap toEnum [0, 0, 1, 1,    1, 0, 0, 0,    0, 0, 0, 0,    0, 1, 0, 0]
    c         = fmap toEnum [0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0]
    d         = fmap toEnum [1, 1, 1, 1,    1, 1, 1, 1,    1, 1, 1, 1,    1, 1, 1, 1]
    pins      = [a, b, c, d]
    inputs    = [(a, b, c, d, (0, 0), a)
                ,(a, b, c, d, (0, 1), b)
                ,(a, b, c, d, (1, 0), c)
                ,(a, b, c, d, (1, 1), d)]
    f         = \(a, b, c, d, (s1, s2), exp) -> (Chips.mux4WayN a b c d (toEnum s1, toEnum s2)) /= exp
    result    = List.find f inputs
    in (HUnit.TestCase $ HUnit.assertEqual ("mux4wayN should validate") Nothing result)

testMux8WayN :: HUnit.Test
testMux8WayN = let
    a         = fmap toEnum [0, 1, 0, 0,    0, 1, 0, 1,    1, 1, 1, 1,    0, 1, 0, 1]
    b         = fmap toEnum [0, 0, 1, 1,    1, 0, 0, 0,    0, 0, 0, 0,    0, 1, 0, 0]
    c         = fmap toEnum [0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0]
    d         = fmap toEnum [1, 1, 1, 1,    1, 1, 1, 1,    1, 1, 1, 1,    1, 1, 1, 1]
    e         = fmap toEnum [1, 1, 0, 0,    0, 0, 1, 1,    1, 1, 0, 0,    1, 1, 0, 0]
    f         = fmap toEnum [0, 0, 1, 1,    1, 1, 0, 0,    0, 0, 1, 1,    0, 0, 1, 1]
    g         = fmap toEnum [0, 0, 0, 0,    1, 1, 1, 1,    0, 0, 0, 0,    1, 1, 1, 1]
    h         = fmap toEnum [1, 1, 1, 1,    0, 0, 0, 0,    1, 1, 1, 1,    0, 0, 0, 0]
    pins      = [a, b, c, d, e, f, g, h]
    inputs    = [(a, b, c, d, e, f, g, h, (0, 0, 0), a)
                ,(a, b, c, d, e, f, g, h, (0, 0, 1), b)
                ,(a, b, c, d, e, f, g, h, (0, 1, 0), c)
                ,(a, b, c, d, e, f, g, h, (0, 1, 1), d)
                ,(a, b, c, d, e, f, g, h, (1, 0, 0), e)
                ,(a, b, c, d, e, f, g, h, (1, 0, 1), f)
                ,(a, b, c, d, e, f, g, h, (1, 1, 0), g)
                ,(a, b, c, d, e, f, g, h, (1, 1, 1), h)]
    fn        = \(a, b, c, d, e, f, g, h, (s1, s2, s3), exp) -> (Chips.mux8WayN a b c d e f g h (toEnum s1, toEnum s2, toEnum s3)) /= exp
    result    = List.find fn inputs
    in (HUnit.TestCase $ HUnit.assertEqual ("mux8wayN should validate") Nothing result)

testDmux4Way :: HUnit.Test
testDmux4Way = let
    values    = [((0, 0), 0, (0, 0, 0, 0))
                ,((0, 1), 0, (0, 0, 0, 0))
                ,((1, 0), 0, (0, 0, 0, 0))
                ,((1, 1), 0, (0, 0, 0, 0))
                ,((0, 0), 1, (1, 0, 0, 0))
                ,((0, 1), 1, (0, 1, 0, 0))
                ,((1, 0), 1, (0, 0, 1, 0))
                ,((1, 1), 1, (0, 0, 0, 1))]
    enumify   = \((s1, s2), i, (a, b, c, d)) -> ((toEnum s1, toEnum s2), toEnum i, (toEnum a, toEnum b, toEnum c, toEnum d)) :: ((Bool, Bool), Bool, (Bool, Bool, Bool, Bool))
    values'   = fmap enumify values
    f         = \((sel1, sel2), i, expected) -> (Chips.dmux4Way i (sel1, sel2)) /= expected
    result    = List.find f values'
    in (HUnit.TestCase $ HUnit.assertEqual ("dmux4Way should validate") Nothing result)

testDmux8Way :: HUnit.Test
testDmux8Way = let
    values    = [((0, 0, 0), 0, (0, 0, 0, 0, 0, 0, 0, 0))
                ,((0, 0, 1), 0, (0, 0, 0, 0, 0, 0, 0, 0))
                ,((0, 1, 0), 0, (0, 0, 0, 0, 0, 0, 0, 0))
                ,((0, 1, 1), 0, (0, 0, 0, 0, 0, 0, 0, 0))
                ,((1, 0, 0), 0, (0, 0, 0, 0, 0, 0, 0, 0))
                ,((1, 0, 1), 0, (0, 0, 0, 0, 0, 0, 0, 0))
                ,((1, 1, 0), 0, (0, 0, 0, 0, 0, 0, 0, 0))
                ,((1, 1, 1), 0, (0, 0, 0, 0, 0, 0, 0, 0))
                ,((0, 0, 0), 1, (1, 0, 0, 0, 0, 0, 0, 0))
                ,((0, 0, 1), 1, (0, 1, 0, 0, 0, 0, 0, 0))
                ,((0, 1, 0), 1, (0, 0, 1, 0, 0, 0, 0, 0))
                ,((0, 1, 1), 1, (0, 0, 0, 1, 0, 0, 0, 0))
                ,((1, 0, 0), 1, (0, 0, 0, 0, 1, 0, 0, 0))
                ,((1, 0, 1), 1, (0, 0, 0, 0, 0, 1, 0, 0))
                ,((1, 1, 0), 1, (0, 0, 0, 0, 0, 0, 1, 0))
                ,((1, 1, 1), 1, (0, 0, 0, 0, 0, 0, 0, 1))]
    enumify   = \((s1, s2, s3), i, (a, b, c, d, e, f, g, h)) -> ((toEnum s1, toEnum s2, toEnum s3), toEnum i, (toEnum a, toEnum b, toEnum c, toEnum d, toEnum e, toEnum f, toEnum g, toEnum h)) :: ((Bool, Bool, Bool), Bool, (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool))
    values'   = fmap enumify values
    f         = \((sel1, sel2, sel3), i, expected) -> (Chips.dmux8Way i (sel1, sel2, sel3)) /= expected
    result    = List.find f values'
    in (HUnit.TestCase $ HUnit.assertEqual ("dmux8Way should validate") Nothing result)

chipTests :: [HUnit.Test]
chipTests = [testChipsNand
            ,testChipsAnd
            ,testChipsNot
            ,testChipsOr
            ,testChipsXor
            ,testChipsMux
            ,testChipsDMux
            ,testNotN
            ,testAndN
            ,testOrN
            ,testMuxNa
            ,testMuxNb
            ,testOrNWay
            ,testMux4WayN
            ,testMux8WayN
            ,testDmux4Way
            ,testDmux8Way]
