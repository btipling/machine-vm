module Main where

import qualified Machine.Gates as Gates

main :: IO ()
main = do
  putStrLn $ "nand: " ++ (show $ Gates.nand (toEnum 0) (toEnum 1))
