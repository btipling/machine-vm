module Machine.ALU (halfAdder
                   ,fullAdder
                   ,addN
                   ,incN
                   ,alu) where

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
    ab = zip a b
    f  = Tuple.uncurry halfAdder
    in (fmap snd (fmap f ab))

incN :: Chips.Mpin -> Chips.Mpin
incN a = addN a (iterate (\_ -> True) True)

alu :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Chips.Mpin -> Chips.Mpin -> (Chips.Mpin, Bool, Bool)
alu zx nx zy ny f no x y = let
    zero   = \input sel -> fmap (\a -> Chips.and (Chips.not sel) a) input
    negate = \input sel -> fmap (\a -> snd (halfAdder a sel)) input
    x'z    = zero x     zx  -- x'z is zeros if zx is set
    x'zn   = negate x'z nx  -- x'zn is negated if nx is set
    y'z    = zero y zy      -- y'z is zeroes if zy is set
    y'zn   = negate y'z ny  -- y'zn is negated if ny is set
    yz     = zip x'zn y'zn
    func   = \(a, b) -> Chips.or (Chips.and f (snd (halfAdder a b))) (Chips.and (Chips.not f) (Chips.and a b))
    out    = fmap func yz
    out'n  = negate out no  -- output is negated if no is set
    zr     = Chips.not (foldl (Chips.or) False out'n)
    ng     = head out'n
    in ((out'n, zr, ng))
