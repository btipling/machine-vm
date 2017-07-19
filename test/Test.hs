module Main where

import qualified Data.List     as List
import qualified Machine.Gates as Gates
import qualified Test.HUnit    as HUnit

testGate :: String -> (Int -> Int -> Int) -> [(Int, Int, Int)] -> HUnit.Test
testGate name gate values = let
    result = List.find (\(a, b, expectedOutcome) -> (gate a b) /= expectedOutcome) values
    in (HUnit.TestCase $ HUnit.assertEqual (name ++ " should validate") Nothing result)

testGatesNot :: HUnit.Test
testGatesNot = let
    values = [(0, 1)
             ,(1, 0)]
    result = List.find (\(a, expectedOutcome) -> (Gates.not a) /= expectedOutcome) values
    in (HUnit.TestCase $ HUnit.assertEqual ("not should validate") Nothing result)

testGatesNand :: HUnit.Test
testGatesNand = testGate "Nand" Gates.nand [(0, 0, 1)
                                           ,(1, 0, 1)
                                           ,(0, 1, 1)
                                           ,(1, 1, 0)]

testGatesAnd :: HUnit.Test
testGatesAnd = testGate "And" Gates.and [(0, 0, 0)
                                        ,(1, 0, 0)
                                        ,(0, 1, 0)
                                        ,(1, 1, 1)]

testGatesOr :: HUnit.Test
testGatesOr = testGate "Or" Gates.or [(0, 0, 0)
                                     ,(1, 0, 1)
                                     ,(0, 1, 1)
                                     ,(1, 1, 1)]

testGatesXor :: HUnit.Test
testGatesXor = testGate "Xor" Gates.xor [(0, 0, 0)
                                        ,(1, 0, 1)
                                        ,(0, 1, 1)
                                        ,(1, 1, 0)]

main :: IO HUnit.Counts
main = HUnit.runTestTT $ HUnit.TestList [testGatesNand
                                        ,testGatesAnd
                                        ,testGatesNot
                                        ,testGatesOr
                                        ,testGatesXor]
