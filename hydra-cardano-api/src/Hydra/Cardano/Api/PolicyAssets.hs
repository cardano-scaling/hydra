{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.PolicyAssets where

import Cardano.Api (PolicyAssets (..))
import Data.Aeson (FromJSON (..), ToJSON (..))

-- * Orphans

instance ToJSON PolicyAssets where
  toJSON (PolicyAssets assets) = toJSON assets

instance FromJSON PolicyAssets where
  parseJSON v = PolicyAssets <$> parseJSON v
