module Machine.ALU (halfAdder
                   ,fullAdder
                   ,addN
                   ,incN
                   ,alu) where

import qualified Data.Tuple    as Tuple
import qualified Machine.Gates as Gates

halfAdder :: Bool -> Bool -> (Bool, Bool)
halfAdder a b = let
    carry = Gates.and a b
    sum   = Gates.xor a b
    in ((carry, sum))

fullAdder :: Bool -> Bool -> Bool -> (Bool, Bool)
fullAdder a b c = let
    (abCarry, abSum)    = halfAdder a b
    (abcCarry, abcSum)  = halfAdder abSum c
    carry               = Gates.or abCarry abcCarry
    sum                 = abcSum
    in ((carry, sum))

addN :: Gates.Mpin -> Gates.Mpin -> Gates.Mpin
addN a b = let
    ab = zip a b
    f  = Tuple.uncurry halfAdder
    in (fmap snd (fmap f ab))

incN :: Gates.Mpin -> Gates.Mpin
incN a = addN a (iterate (\_ -> True) True)

-- The alu has two bit arrays that are manipulated based on the various flags
-- the two bit arrays x and y may be negated or zeroed out and then either and'ed or added together
-- the addition is two's complement with no carry
-- there is a flag to negate the output
alu :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Gates.Mpin -> Gates.Mpin -> (Gates.Mpin, Bool, Bool)
alu zx nx zy ny f no x y = let
    zero   = \input sel -> fmap (\a -> Gates.and (Gates.not sel) a) input
    negate = \input sel -> fmap (\a -> snd (halfAdder a sel)) input
    x'z    = zero x     zx  -- x'z is zeros if zx is set
    x'zn   = negate x'z nx  -- x'zn is negated if nx is set
    y'z    = zero y zy      -- y'z is zeroes if zy is set
    y'zn   = negate y'z ny  -- y'zn is negated if ny is set
    yz     = zip x'zn y'zn
    func   = \(a, b) -> Gates.or (Gates.and f (snd (halfAdder a b))) (Gates.and (Gates.not f) (Gates.and a b))
    out    = fmap func yz
    out'n  = negate out no  -- output is negated if no is set
    zr     = Gates.not (foldl (Gates.or) False out'n)
    ng     = head out'n
    in ((out'n, zr, ng))
