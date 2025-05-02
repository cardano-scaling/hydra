{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.Orphans.NetworkMagic where

import Cardano.Api (NetworkMagic (..))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Test.Gen.Cardano.Api.Typed (genNetworkMagic)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Hedgehog (hedgehog)

instance ToJSON NetworkMagic where
  toJSON (NetworkMagic magic) = toJSON magic

instance FromJSON NetworkMagic where
  parseJSON = fmap NetworkMagic . parseJSON

instance Arbitrary NetworkMagic where
  arbitrary = hedgehog genNetworkMagic
  shrink (NetworkMagic x) = NetworkMagic <$> shrink x
