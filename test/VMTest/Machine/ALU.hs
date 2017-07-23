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

aluTests :: [HUnit.Test]
aluTests = [halfAdderTest
           ,fullAdderTest
           ,addTest
           ,incTest]
