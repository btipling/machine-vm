module Machine.Memory (dff
                      ,bitRegister) where

import qualified Control.Monad.State as State
import qualified Machine.Gates       as Gates

dff :: Bool -> State.State Bool Bool
dff n = State.state (\s -> (s, n))

bitRegister :: Bool -> Bool -> State.State Bool Bool
bitRegister input load = do
    currentState <- State.get
    dff $ Gates.mux currentState input load
