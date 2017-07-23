module Machine.ALU (halfAdder
                   ,fullAdder
                   ,addN
                   ,incN) where

import qualified Data.Tuple    as Tuple
import qualified Machine.Chips as Chips

halfAdder :: Bool -> Bool -> (Bool, Bool)
halfAdder a b = let
    carry = Chips.and a b
    sum   = Chips.xor a b
    in ((carry, sum))

fullAdder :: Bool -> Bool -> Bool -> (Bool, Bool)
fullAdder a b c = let
    (abCarry, abSum)    = halfAdder a b
    (abcCarry, abcSum)  = halfAdder abSum c
    carry               = Chips.or abCarry abcCarry
    sum                 = abcSum
    in ((carry, sum))

addN :: Chips.Mpin -> Chips.Mpin -> Chips.Mpin
addN a b = let
    ab    = zip a b
    f     = Tuple.uncurry halfAdder
    pairs = fmap f ab
    sums  = fmap (\(curry, sum) -> sum) pairs
    in (sums)

incN :: Chips.Mpin -> Chips.Mpin
incN a = addN a (iterate (\_ -> True) True)
