module Machine.Gates where

nand :: Int -> Int -> Int
nand a b | a == 0 && b == 0 = 1
         | a == 1 && b == 0 = 1
         | a == 0 && b == 1 = 1
         | a == 1 && b == 1 = 0
