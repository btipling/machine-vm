module Machine.Chips (nand, not, and, or, xor, mux) where

import           Prelude hiding (and, not, or)

-- The goal is to define a nand gate, and to use the nand for all further logic and
-- not use built in Haskell language features that serve the same purpose beyond the
-- nand gate, at least as much as possible. In the future, to improve performance
-- this may change.
nand :: Bool -> Bool -> Bool
nand a b | a && b    = False
         | otherwise = True

not :: Bool -> Bool
not a = nand a a

and :: Bool -> Bool -> Bool
and a b = not (nand a b)

or :: Bool -> Bool -> Bool
or a b = nand (not a) (not b)

xor :: Bool -> Bool -> Bool
xor a b = nand (nand a (nand a b)) (nand b (nand a b))

mux :: Bool -> Bool -> Bool -> Bool
mux a b sel = or (and (not sel) a) (and sel b)

-- Returning information on two pins required something like the let construct.
dmux :: Bool -> Bool -> (Bool, Bool)
dmux sel input = let
    a = and (not sel) input
    b = and sel input
    in ((a, b))
