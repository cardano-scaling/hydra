{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.NetworkMagic where

import "aeson" Data.Aeson (FromJSON (..), ToJSON (..))
import "cardano-api" Cardano.Api (NetworkMagic (..))

-- * Orphans

instance ToJSON NetworkMagic where
  toJSON (NetworkMagic magic) = toJSON magic

instance FromJSON NetworkMagic where
  parseJSON = fmap NetworkMagic . parseJSON
