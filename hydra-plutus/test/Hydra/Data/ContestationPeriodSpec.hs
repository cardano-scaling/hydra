{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Data.ContestationPeriodSpec where

import Hydra.Prelude

import Hydra.Data.ContestationPeriod (
  ContestationPeriod,
  contestationPeriodFromDiffTime,
  contestationPeriodToDiffTime,
 )
import Hydra.Plutus.Gen ()
import Test.Hspec (Spec, around_, describe)
import Test.Hspec.QuickCheck (prop)
import Test.Hydra.Prelude (nightlyRuns, onlyNightly)
import Test.QuickCheck (Property, withMaxSuccess, (===))

spec :: Spec
spec = do
  describe "to/from NominalDiffTime" $
    prop "is isomorphic to NominalDiffTime" prop_isomorphicToNominalDiffTime

  around_ onlyNightly $
    describe "to/from NominalDiffTime (deep) @nightly" $
      prop "is isomorphic to NominalDiffTime" $
        withMaxSuccess nightlyRuns prop_isomorphicToNominalDiffTime

-- | 'ContestationPeriod' roundtrips through 'NominalDiffTime'.
prop_isomorphicToNominalDiffTime :: ContestationPeriod -> Property
prop_isomorphicToNominalDiffTime t =
  let diff = contestationPeriodToDiffTime t
   in contestationPeriodFromDiffTime diff === t
