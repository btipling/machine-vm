module Main where

import qualified System.Exit          as Exit
import qualified Test.HUnit           as HUnit
import qualified VMTest.Machine.ALU   as ALU
import qualified VMTest.Machine.Chips as Chips

main :: IO ()
main = do
    result <- HUnit.runTestTT $ HUnit.TestList (Chips.chipTests ++ ALU.aluTests)
    if (HUnit.failures result) > 0
        then Exit.exitWith $ Exit.ExitFailure 1
        else Exit.exitWith Exit.ExitSuccess
