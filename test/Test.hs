module Main where

import qualified Data.List     as List
import qualified Machine.Gates as Gates
import qualified Test.HUnit    as HUnit

testGatesNand :: HUnit.Test
testGatesNand = let
    values = [(0, 0, 1),
              (1, 0, 1),
              (0, 1, 1),
              (1, 1, 0)]
    result = List.find (\(a, b, expectedOutcome) -> (Gates.nand a b) /= expectedOutcome) values
    in (HUnit.TestCase $ HUnit.assertEqual "Nand should validate" Nothing result)

main :: IO HUnit.Counts
main = HUnit.runTestTT $ HUnit.TestList [testGatesNand]
