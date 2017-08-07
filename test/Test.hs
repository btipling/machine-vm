module Main where

import qualified System.Exit           as Exit
import qualified Test.HUnit            as HUnit
import qualified VMTest.Machine.ALU    as ALU
import qualified VMTest.Machine.Gates  as Gates
import qualified VMTest.Machine.Memory as Memory

main :: IO ()
main = do
    result <- HUnit.runTestTT $ HUnit.TestList (Gates.gateTests
                                             ++ ALU.aluTests
                                             ++ Memory.memoryTests)
    if (HUnit.failures result) > 0
        then Exit.exitWith $ Exit.ExitFailure 1
        else Exit.exitWith Exit.ExitSuccess
