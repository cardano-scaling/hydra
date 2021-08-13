module Hydra.TUISpec where

import Hydra.Prelude (Bool (True), ($))

import Test.Hspec (Spec, it, parallel)

spec :: Spec
spec = parallel $
  it "exists" True
