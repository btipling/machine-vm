module Main where

import qualified Data.Bits     as Bits
import qualified Data.List     as List
import qualified Machine.Chips as Chips
import qualified System.Exit   as Exit
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

main :: IO HUnit.Counts
main = do
    result <- HUnit.runTestTT $ HUnit.TestList [testChipsNand
                                        ,testChipsAnd
                                        ,testChipsNot
                                        ,testChipsOr
                                        ,testChipsXor]
    if (HUnit.failures result) > 0
        then Exit.exitWith $ Exit.ExitFailure 1
        else return result
