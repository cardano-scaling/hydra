{-# LANGUAGE TypeApplications #-}

module Hydra.ModelSpec where

import Control.Monad.IOSim (IOSim)
import Hydra.Model (WorldState)
import Hydra.Prelude
import Test.Hydra.Prelude
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (monadic, monadicIO)
import Test.QuickCheck.StateModel (Actions, runActions)

spec :: Spec
spec =
  prop "implementation respects model" $ prop_checkModel

prop_checkModel :: Actions (WorldState (IOSim s)) -> Property
prop_checkModel actions =
  monadic _foo $ runActions actions
