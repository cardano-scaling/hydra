{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hydra.Cardano.Api.Address where

import Hydra.Cardano.Api.Prelude

-- * Orphans

instance ToJSON (Address ByronAddr) where
  toJSON = error "toJSON"

instance FromJSON (Address ByronAddr) where
  parseJSON = error "parseJSON"

instance Arbitrary (Address ByronAddr) where
  arbitrary = error "arbitrary"
