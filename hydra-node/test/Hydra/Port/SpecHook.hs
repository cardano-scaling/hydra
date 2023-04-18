module Hydra.Port.SpecHook where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Monad.Class.MonadSTM (newTVarIO)

-- | Hspec hook to re-use the same 'TVar' for holding the list of all used ports.
-- This is needed for the tests that depend on having a free port.
hook :: SpecWith (TVar IO [Int]) -> Spec
hook = beforeAll (newTVarIO [])
