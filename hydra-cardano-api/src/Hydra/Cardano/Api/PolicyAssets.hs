{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.PolicyAssets where

import Hydra.Cardano.Api.Prelude

-- * Orphans

instance ToJSON PolicyAssets where
  toJSON (PolicyAssets assets) = toJSON assets

instance FromJSON PolicyAssets where
  parseJSON v = PolicyAssets <$> parseJSON v

-- missing CBOR instances

instance ToCBOR AssetName where
  toCBOR = toCBOR . serialiseToRawBytes

instance FromCBOR AssetName where
  fromCBOR = do
    bs <- fromCBOR
    case deserialiseFromRawBytes AsAssetName bs of
      Left err -> fail (show err)
      Right v -> pure v

instance ToCBOR Quantity where
  toCBOR (Quantity q) = toCBOR q

instance FromCBOR Quantity where
  fromCBOR = Quantity <$> fromCBOR

instance ToCBOR PolicyAssets where
  toCBOR (PolicyAssets assets) = toCBOR assets

instance FromCBOR PolicyAssets where
  fromCBOR = PolicyAssets <$> fromCBOR
