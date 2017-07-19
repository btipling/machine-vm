module Machine.Gates (nand, not, and, or) where

import           Prelude hiding (and, not, or)

nand :: Int -> Int -> Int
nand a b | a == 0 && b == 0 = 1
         | a == 1 && b == 0 = 1
         | a == 0 && b == 1 = 1
         | a == 1 && b == 1 = 0

not :: Int -> Int
not a = nand 1 a

and :: Int -> Int -> Int
and a b = not (nand a b)

or :: Int -> Int -> Int
or a b = and (nand a b) (not (and (not a) (not b)))
