{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.NetworkMagic where

import Cardano.Api (FromCBOR (..), NetworkMagic (..), ToCBOR (..))
import Data.Aeson (FromJSON (..), ToJSON (..))

-- * Orphans

instance ToJSON NetworkMagic where
  toJSON (NetworkMagic magic) = toJSON magic

instance FromJSON NetworkMagic where
  parseJSON = fmap NetworkMagic . parseJSON

-- NOTE: Encoded as the bare 'Word32' magic, consistent with the JSON
-- representation.
instance ToCBOR NetworkMagic where
  toCBOR (NetworkMagic magic) = toCBOR magic

instance FromCBOR NetworkMagic where
  fromCBOR = NetworkMagic <$> fromCBOR
