module Main where

import qualified Data.List     as List
import qualified Machine.Chips as Chips
import qualified Test.HUnit    as HUnit

testGate :: String -> (Int -> Int -> Int) -> [(Int, Int, Int)] -> HUnit.Test
testGate name gate values = let
    result = List.find (\(a, b, expectedOutcome) -> (gate a b) /= expectedOutcome) values
    in (HUnit.TestCase $ HUnit.assertEqual (name ++ " should validate") Nothing result)

testChipsNot :: HUnit.Test
testChipsNot = let
    values = [(0, 1)
             ,(1, 0)]
    result = List.find (\(a, expectedOutcome) -> (Chips.not a) /= expectedOutcome) values
    in (HUnit.TestCase $ HUnit.assertEqual ("not should validate") Nothing result)

testChipsNand :: HUnit.Test
testChipsNand = testGate "Nand" Chips.nand [(0, 0, 1)
                                           ,(1, 0, 1)
                                           ,(0, 1, 1)
                                           ,(1, 1, 0)]

testChipsAnd :: HUnit.Test
testChipsAnd = testGate "And" Chips.and [(0, 0, 0)
                                        ,(1, 0, 0)
                                        ,(0, 1, 0)
                                        ,(1, 1, 1)]

testChipsOr :: HUnit.Test
testChipsOr = testGate "Or" Chips.or [(0, 0, 0)
                                     ,(1, 0, 1)
                                     ,(0, 1, 1)
                                     ,(1, 1, 1)]

testChipsXor :: HUnit.Test
testChipsXor = testGate "Xor" Chips.xor [(0, 0, 0)
                                        ,(1, 0, 1)
                                        ,(0, 1, 1)
                                        ,(1, 1, 0)]

main :: IO HUnit.Counts
main = HUnit.runTestTT $ HUnit.TestList [testChipsNand
                                        ,testChipsAnd
                                        ,testChipsNot
                                        ,testChipsOr
                                        ,testChipsXor]
