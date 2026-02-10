{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Data.ContestationPeriodSpec where

import "hydra-prelude" Hydra.Prelude
import "QuickCheck" Test.QuickCheck ((===))
import "hspec" Test.Hspec (Spec, describe)
import "hspec" Test.Hspec.QuickCheck (prop)

import Hydra.Data.ContestationPeriod (
  contestationPeriodFromDiffTime,
  contestationPeriodToDiffTime,
 )
import Hydra.Plutus.Gen ()

spec :: Spec
spec = do
  describe "to/from NominalDiffTime" $
    prop "is isomorphic to NominalDiffTime" $ \t ->
      let diff = contestationPeriodToDiffTime t
       in contestationPeriodFromDiffTime diff === t
