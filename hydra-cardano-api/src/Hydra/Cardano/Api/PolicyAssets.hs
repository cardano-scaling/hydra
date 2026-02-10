{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.PolicyAssets where

import "hydra-cardano-api" Hydra.Cardano.Api.Prelude

-- * Orphans

instance ToJSON PolicyAssets where
  toJSON (PolicyAssets assets) = toJSON assets

instance FromJSON PolicyAssets where
  parseJSON v = PolicyAssets <$> parseJSON v
