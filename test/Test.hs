module Main where

import qualified Machine.Gates
import qualified Test.HUnit    as HUnit

testExample :: HUnit.Test
testExample =
    HUnit.TestCase $ HUnit.assertEqual "True should be True" True True

main :: IO HUnit.Counts
main = HUnit.runTestTT $ HUnit.TestList [testExample]
