module Main where

import qualified Machine.Chips as Chips

main :: IO ()
main = do
  putStrLn $ "nand: " ++ (show $ Chips.nand (toEnum 0) (toEnum 1))
