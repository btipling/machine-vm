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

main :: IO ()
main = do
    result <- HUnit.runTestTT $ HUnit.TestList [testChipsNand
                                               ,testChipsAnd
                                               ,testChipsNot
                                               ,testChipsOr
                                               ,testChipsXor
                                               ,testChipsMux
                                               ,testChipsDMux]
    if (HUnit.failures result) > 0
        then Exit.exitWith $ Exit.ExitFailure 1
        else Exit.exitWith Exit.ExitSuccess
