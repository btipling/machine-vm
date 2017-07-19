module Machine.Chips (nand, not, and, or, xor) where

import           Prelude hiding (and, not, or)

nand :: Int -> Int -> Int
nand a b | a == 0 && b == 0 = 1
         | a == 1 && b == 0 = 1
         | a == 0 && b == 1 = 1
         | a == 1 && b == 1 = 0

not :: Int -> Int
not a = nand a a

and :: Int -> Int -> Int
and a b = not (nand a b)

or :: Int -> Int -> Int
or a b = nand (nand a a) (nand b b)

xor :: Int -> Int -> Int
xor a b = and (or a b) (not (and a b))