module Machine.Gates where

nand :: (Int, Int) -> Int
nand vals | vals == (0, 0) = 1
          | vals == (1, 0) = 1
          | vals == (0, 1) = 1
          | vals == (1, 1) = 0
