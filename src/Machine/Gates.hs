module Machine.Gates (nand, and, or) where

import           Prelude hiding (and, or)

nand :: Int -> Int -> Int
nand a b | a == 0 && b == 0 = 1
         | a == 1 && b == 0 = 1
         | a == 0 && b == 1 = 1
         | a == 1 && b == 1 = 0

and :: Int -> Int -> Int
and a b = nand 1 (nand a b)

or :: Int -> Int -> Int
or a b = nand (and a (nand a b)) (and b (nand a b))
