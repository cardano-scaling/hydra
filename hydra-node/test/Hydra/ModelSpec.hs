module Hydra.ModelSpec where

import Hydra.Model (prop_checkModel)
import Test.Hydra.Prelude

spec :: Spec
spec =
  prop "implementation respects model" prop_checkModel
