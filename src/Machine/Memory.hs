module Machine.Memory (dff) where

import qualified Control.Monad.State as State

dff :: Bool -> State.State Bool Bool
dff n = State.state (\s -> (s, n))
