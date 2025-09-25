{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hydra.Cardano.Api.Address where

import Cardano.Api (Address, ByronAddr)

import Test.Gen.Cardano.Api.Typed (genAddressByron)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Hedgehog (hedgehog)

-- * Orphans

instance Arbitrary (Address ByronAddr) where
  arbitrary = hedgehog genAddressByron
