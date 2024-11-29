{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hydra.Cardano.Api.Address where

import Hydra.Cardano.Api.Prelude

import Test.Cardano.Chain.Common.Gen (genAddress)
import Test.QuickCheck.Hedgehog (hedgehog)

-- * Orphans

instance Arbitrary (Address ByronAddr) where
  arbitrary = ByronAddress <$> hedgehog genAddress
