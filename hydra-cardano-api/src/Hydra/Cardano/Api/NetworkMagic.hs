{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.NetworkMagic where

import Cardano.Api (NetworkMagic (..))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Test.QuickCheck (Arbitrary (..))

-- * Orphans

instance ToJSON NetworkMagic where
  toJSON (NetworkMagic magic) = toJSON magic

instance FromJSON NetworkMagic where
  parseJSON = fmap NetworkMagic . parseJSON

instance Arbitrary NetworkMagic where
  arbitrary = NetworkMagic <$> arbitrary
  shrink (NetworkMagic x) = NetworkMagic <$> shrink x
