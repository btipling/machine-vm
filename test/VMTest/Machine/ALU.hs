module VMTest.Machine.ALU (aluTests) where

import qualified Data.List   as List
import qualified Machine.ALU as ALU
import qualified Test.HUnit  as HUnit

halfAdderTest :: HUnit.Test
halfAdderTest = let
    values = [((0, 0), (0, 0))
             ,((0, 1), (0, 1))
             ,((1, 0), (0, 1))
             ,((1, 1), (1, 0))]
    f      = \((a, b), (carry, sum)) -> ALU.halfAdder (toEnum a) (toEnum b) /= (toEnum carry, toEnum sum)
    result = List.find f values
    in (HUnit.TestCase $ HUnit.assertEqual "halfAdder should validate" Nothing result)

fullAdderTest :: HUnit.Test
fullAdderTest = let
    values = [((0, 0, 0), (0, 0))
             ,((0, 0, 1), (0, 1))
             ,((0, 1, 0), (0, 1))
             ,((0, 1, 1), (1, 0))
             ,((1, 0, 0), (0, 1))
             ,((1, 0, 1), (1, 0))
             ,((1, 1, 0), (1, 0))
             ,((1, 1, 1), (1, 1))]
    f      = \((a, b, c), (carry, sum)) -> ALU.fullAdder (toEnum a) (toEnum b) (toEnum c) /= (toEnum carry, toEnum sum)
    result = List.find f values
    in (HUnit.TestCase $ HUnit.assertEqual "fullAdder should validate" Nothing result)

addTest :: HUnit.Test
addTest = let
    (a, b, expected) = ( fmap toEnum [ 0, 0, 0, 0,    1, 1, 1, 1,    0, 0, 0, 0,    1, 0, 1, 0 ]
                       , fmap toEnum [ 0, 0, 0, 0,    1, 1, 1, 1,    1, 1, 1, 1,    0, 1, 1, 0 ]
                       , fmap toEnum [ 0, 0, 0, 0,    0, 0, 0, 0,    1, 1, 1, 1,    1, 1, 0, 0 ])
    result           = ALU.addN a b
    in (HUnit.TestCase $ HUnit.assertEqual "addN should validate" expected result)

incTest :: HUnit.Test
incTest = let
    (a, expected) = ( fmap toEnum [ 0, 0, 0, 0,    1, 1, 1, 1,    0, 0, 0, 0,    1, 0, 1, 0 ]
                    , fmap toEnum [ 1, 1, 1, 1,    0, 0, 0, 0,    1, 1, 1, 1,    0, 1, 0, 1 ])
    result        = ALU.incN a
    in (HUnit.TestCase $ HUnit.assertEqual "incN should validate" expected result)

aluAndTest :: HUnit.Test
aluAndTest = let
    zx          = toEnum 0
    nx          = toEnum 0
    zy          = toEnum 0
    ny          = toEnum 0
    f           = toEnum 0
    no          = toEnum 0
    x           = fmap toEnum [ 0, 0, 0, 1,    1, 1, 1, 1,    1, 1, 1, 1,    1, 1, 1, 1 ]
    y           = fmap toEnum [ 0, 1, 0, 1,    1, 0, 1, 0,    0, 0, 0, 0,    1, 1, 1, 1 ]
    expectedOut = fmap toEnum [ 0, 0, 0, 1,    1, 0, 1, 0,    0, 0, 0, 0,    1, 1, 1, 1 ]
    (out, _, _) = ALU.alu zx nx zy ny f no x y
    in (HUnit.TestCase $ HUnit.assertEqual "alu and should validate" expectedOut out)

aluAddTest :: HUnit.Test
aluAddTest = let
    zx          = toEnum 0
    nx          = toEnum 0
    zy          = toEnum 0
    ny          = toEnum 0
    f           = toEnum 1
    no          = toEnum 0
    x           = fmap toEnum [ 0, 0, 0, 1,    1, 1, 1, 1,    1, 1, 1, 1,    1, 1, 1, 1 ]
    y           = fmap toEnum [ 0, 1, 0, 1,    1, 0, 1, 0,    0, 0, 0, 0,    1, 1, 1, 1 ]
    expectedOut = fmap toEnum [ 0, 1, 0, 0,    0, 1, 0, 1,    1, 1, 1, 1,    0, 0, 0, 0 ]
    (out, _, _) = ALU.alu zx nx zy ny f no x y
    in (HUnit.TestCase $ HUnit.assertEqual "alu add should validate" expectedOut out)

aluZrNotZeroTest :: HUnit.Test
aluZrNotZeroTest = let
    zx          = toEnum 0
    nx          = toEnum 0
    zy          = toEnum 0
    ny          = toEnum 0
    f           = toEnum 1
    no          = toEnum 0
    x           = fmap toEnum [ 0, 0, 0, 1,    1, 1, 1, 1,    1, 1, 1, 1,    1, 1, 1, 1 ]
    y           = fmap toEnum [ 0, 1, 0, 1,    1, 0, 1, 0,    0, 0, 0, 0,    1, 1, 1, 1 ]
    expectedZr  = toEnum 0
    (_, zr, _) = ALU.alu zx nx zy ny f no x y
    in (HUnit.TestCase $ HUnit.assertEqual "alu zr not zero should validate" expectedZr zr)

aluZrZeroTest :: HUnit.Test
aluZrZeroTest = let
    zx          = toEnum 0
    nx          = toEnum 0
    zy          = toEnum 0
    ny          = toEnum 0
    f           = toEnum 0
    no          = toEnum 0
    x           = fmap toEnum [ 0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0 ]
    y           = fmap toEnum [ 0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0 ]
    expectedZr  = toEnum 1
    (_, zr, _) = ALU.alu zx nx zy ny f no x y
    in (HUnit.TestCase $ HUnit.assertEqual "alu zr zero should validate" expectedZr zr)

aluNgNegativeTest :: HUnit.Test
aluNgNegativeTest = let
    zx          = toEnum 0
    nx          = toEnum 0
    zy          = toEnum 0
    ny          = toEnum 0
    f           = toEnum 1
    no          = toEnum 0
    x           = fmap toEnum [ 1, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 1 ]
    y           = fmap toEnum [ 0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0 ]
    expectedNg  = toEnum 1
    (_, _, ng) = ALU.alu zx nx zy ny f no x y
    in (HUnit.TestCase $ HUnit.assertEqual "alu ng negative should be true" expectedNg ng)

aluNgPositiveTest :: HUnit.Test
aluNgPositiveTest = let
    zx          = toEnum 0
    nx          = toEnum 0
    zy          = toEnum 0
    ny          = toEnum 0
    f           = toEnum 1
    no          = toEnum 0
    x           = fmap toEnum [ 0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 1 ]
    y           = fmap toEnum [ 0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0 ]
    expectedNg  = toEnum 0
    (_, _, ng) = ALU.alu zx nx zy ny f no x y
    in (HUnit.TestCase $ HUnit.assertEqual "alu ng negative should be false" expectedNg ng)

aluZeroXTest :: HUnit.Test
aluZeroXTest = let
    zx          = toEnum 1
    nx          = toEnum 0
    zy          = toEnum 0
    ny          = toEnum 0
    f           = toEnum 0
    no          = toEnum 0
    x           = fmap toEnum [ 1, 1, 1, 1,    1, 1, 1, 1,    1, 1, 1, 1,    1, 1, 1, 1 ]
    y           = fmap toEnum [ 1, 1, 1, 1,    1, 1, 1, 1,    1, 1, 1, 1,    1, 1, 1, 1 ]
    expectedZr  = toEnum 1
    (_, zr, _) = ALU.alu zx nx zy ny f no x y
    in (HUnit.TestCase $ HUnit.assertEqual "alu zero x and should result in zero" expectedZr zr)

aluZeroYTest :: HUnit.Test
aluZeroYTest = let
    zx          = toEnum 0
    nx          = toEnum 0
    zy          = toEnum 1
    ny          = toEnum 0
    f           = toEnum 0
    no          = toEnum 0
    x           = fmap toEnum [ 1, 1, 1, 1,    1, 1, 1, 1,    1, 1, 1, 1,    1, 1, 1, 1 ]
    y           = fmap toEnum [ 1, 1, 1, 1,    1, 1, 1, 1,    1, 1, 1, 1,    1, 1, 1, 1 ]
    expectedZr  = toEnum 1
    (_, zr, _) = ALU.alu zx nx zy ny f no x y
    in (HUnit.TestCase $ HUnit.assertEqual "alu zero y and should result in zero" expectedZr zr)

aluNotXAddTest :: HUnit.Test
aluNotXAddTest = let
    zx          = toEnum 0
    nx          = toEnum 1
    zy          = toEnum 0
    ny          = toEnum 0
    f           = toEnum 1
    no          = toEnum 0
    x           = fmap toEnum [ 0, 0, 0, 0,    1, 0, 1, 0,    1, 1, 1, 1,    1, 1, 1, 1 ]
    y           = fmap toEnum [ 1, 1, 1, 1,    1, 1, 1, 1,    0, 0, 0, 0,    1, 1, 1, 1 ]
    expectedOut = fmap toEnum [ 0, 0, 0, 0,    1, 0, 1, 0,    0, 0, 0, 0,    1, 1, 1, 1 ]
    (out, _, _) = ALU.alu zx nx zy ny f no x y
    in (HUnit.TestCase $ HUnit.assertEqual "alu not x add should validate" expectedOut out)

aluNotYAndTest :: HUnit.Test
aluNotYAndTest = let
    zx          = toEnum 0
    nx          = toEnum 0
    zy          = toEnum 0
    ny          = toEnum 1
    f           = toEnum 0
    no          = toEnum 0
    x           = fmap toEnum [ 0, 0, 0, 0,    1, 0, 1, 0,    1, 1, 1, 1,    1, 1, 1, 1 ]
    y           = fmap toEnum [ 1, 1, 1, 1,    0, 1, 1, 1,    0, 0, 0, 0,    1, 1, 1, 1 ]
    expectedOut = fmap toEnum [ 0, 0, 0, 0,    1, 0, 0, 0,    1, 1, 1, 1,    0, 0, 0, 0 ]
    (out, _, _) = ALU.alu zx nx zy ny f no x y
    in (HUnit.TestCase $ HUnit.assertEqual "alu not y and should validate" expectedOut out)

aluNotOutTest :: HUnit.Test
aluNotOutTest = let
    zx          = toEnum 0
    nx          = toEnum 0
    zy          = toEnum 0
    ny          = toEnum 0
    f           = toEnum 0
    no          = toEnum 1
    x           = fmap toEnum [ 0, 0, 0, 0,    1, 0, 1, 0,    1, 1, 1, 1,    1, 1, 1, 1 ]
    y           = fmap toEnum [ 1, 1, 1, 1,    0, 1, 1, 1,    0, 0, 0, 0,    1, 1, 1, 1 ]
    expectedOut = fmap toEnum [ 1, 1, 1, 1,    1, 1, 0, 1,    1, 1, 1, 1,    0, 0, 0, 0 ]
    (out, _, _) = ALU.alu zx nx zy ny f no x y
    in (HUnit.TestCase $ HUnit.assertEqual "alu not out should validate" expectedOut out)

aluTests :: [HUnit.Test]
aluTests = [halfAdderTest
           ,fullAdderTest
           ,addTest
           ,incTest
           ,aluAndTest
           ,aluAddTest
           ,aluZrNotZeroTest
           ,aluZrZeroTest
           ,aluNgNegativeTest
           ,aluNgPositiveTest
           ,aluZeroXTest
           ,aluZeroYTest
           ,aluNotXAddTest
           ,aluNotYAndTest
           ,aluNotOutTest]
