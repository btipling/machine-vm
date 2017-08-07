module Machine.Gates (nand
                     ,not
                     ,and
                     ,or
                     ,xor
                     ,mux
                     ,dmux
                     ,notN
                     ,andN
                     ,orN
                     ,muxN
                     ,orNWay
                     ,mux4WayN
                     ,mux8WayN
                     ,dmux4Way
                     ,dmux8Way
                     ,Mpin) where

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

-- Haskell doesn't have fixed width arrays, just tuples which doesn't help much. Using
-- built in arrays as a compromise. This should be a not16, but it's just a notN. This
-- breaks the hardware simulation constraints I wanted to achieve in this instance. You
-- can't have an endless hardware bus in real life. An Mpin is basically a pin array.
type Mpin = [Bool]

notN :: Mpin -> Mpin
notN input = fmap not input

andN :: Mpin -> Mpin -> Mpin
andN a b = fmap (\(a', b') -> and a' b') $ zip a b

orN :: Mpin -> Mpin -> Mpin
orN a b = fmap (\(a', b') -> or a' b') $ zip a b

-- The previous defined boolean gates don't allow any way to return anything other than a Bool
-- A separate mechanism was needed, so falling back to Haskell. In chip designs the chip definition
-- allows the designer mechanisms for defining imperative logic for outs not possible here.
muxN :: Bool -> Mpin -> Mpin -> Mpin
muxN sel a b | sel       = b
             | otherwise = a

orNWay :: Mpin -> Bool
orNWay input = foldl or False input

-- Again I just don't yet see anyway to just use previously defined functions to achieve this chip.
mux4WayN :: Mpin -> Mpin -> Mpin -> Mpin -> (Bool, Bool) -> Mpin
mux4WayN a b c d (sel1, sel2) | and (not sel1) (not sel2) = a -- 00
                              | and (not sel1)      sel2  = b -- 01
                              | and      sel1  (not sel2) = c -- 10
                              | and      sel1       sel2  = d -- 11

mux8WayN :: Mpin -> Mpin -> Mpin -> Mpin -> Mpin -> Mpin -> Mpin -> Mpin -> (Bool, Bool, Bool) -> Mpin
mux8WayN a b c d e f g h (sel1, sel2, sel3) | and (and (not sel1) (not sel2)) (not sel3) = a -- 000
                                            | and (and (not sel1) (not sel2))      sel3  = b -- 001
                                            | and (and (not sel1)      sel2 ) (not sel3) = c -- 010
                                            | and (and (not sel1)      sel2 )      sel3  = d -- 011
                                            | and (and      sel1  (not sel2)) (not sel3) = e -- 100
                                            | and (and      sel1  (not sel2))      sel3  = f -- 101
                                            | and (and      sel1       sel2 ) (not sel3) = g -- 110
                                            | and (and      sel1       sel2 )      sel3  = h -- 111

dmux4Way :: Bool -> (Bool, Bool) -> (Bool, Bool, Bool, Bool)
dmux4Way input (sel1, sel2) = let
    a = and (and (not sel1) (not sel2)) input -- 00
    b = and (and (not sel1)      sel2)  input -- 01
    c = and (and      sel1  (not sel2)) input -- 10
    d = and (and      sel1       sel2)  input -- 11
    in ((a, b, c, d))

dmux8Way :: Bool -> (Bool, Bool, Bool) -> (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
dmux8Way input (sel1, sel2, sel3) = let
    a = and (and (and (not sel1) (not sel2)) (not sel3)) input -- 000
    b = and (and (and (not sel1) (not sel2))      sel3 ) input -- 001
    c = and (and (and (not sel1)      sel2 ) (not sel3)) input -- 010
    d = and (and (and (not sel1)      sel2 )      sel3 ) input -- 011
    e = and (and (and      sel1  (not sel2)) (not sel3)) input -- 100
    f = and (and (and      sel1  (not sel2))      sel3 ) input -- 101
    g = and (and (and      sel1       sel2 ) (not sel3)) input -- 110
    h = and (and (and      sel1       sel2 )      sel3 ) input -- 111
    in ((a, b, c, d, e, f, g, h))
