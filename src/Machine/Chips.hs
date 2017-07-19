module Machine.Chips (nand, not, and, or, xor) where

import           Prelude hiding (and, not, or)

nand :: Bool -> Bool -> Bool
nand a b | a == False && b == False = True
         | a == True  && b == False = True
         | a == False && b == True  = True
         | a == True  && b == True  = False

not :: Bool -> Bool
not a = nand a a

and :: Bool -> Bool -> Bool
and a b = not (nand a b)

or :: Bool -> Bool -> Bool
or a b = nand (not a) (not b)

xor :: Bool -> Bool -> Bool
xor a b = nand (nand a (nand a b)) (nand b (nand a b))


