module Hydra.Data.ContestationPeriodSpec where

import Hydra.Prelude

import Hydra.Data.ContestationPeriod (posixToUTCTime)
import Plutus.Orphans ()
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

spec :: Spec
spec = describe "posixToUTCTime" $ do
  prop "is homorphic w.r.t to Ord" $ \t1 t2 ->
    compare t1 t2 === compare (posixToUTCTime t1) (posixToUTCTime t2)
