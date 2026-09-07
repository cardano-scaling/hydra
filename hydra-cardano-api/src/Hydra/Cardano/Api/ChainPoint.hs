{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ChainPoint where

import Hydra.Cardano.Api.Prelude

import Codec.CBOR.Generic.Tagged (genericFromCBOR, genericToCBOR)
import GHC.Generics (Generic)

-- | Get the chain point corresponding to a given 'BlockHeader'.
getChainPoint :: BlockHeader -> ChainPoint
getChainPoint header =
  ChainPoint slotNo headerHash
 where
  (BlockHeader slotNo headerHash _) = header

-- * Orphans

-- missing CBOR instances

-- NOTE: Encoded as raw hash bytes, consistent with the previous hand-written
-- 'ChainPoint' encoding.
instance ToCBOR (Hash BlockHeader) where
  toCBOR = toCBOR . serialiseToRawBytes

instance FromCBOR (Hash BlockHeader) where
  fromCBOR = do
    bytes <- fromCBOR
    case deserialiseFromRawBytes (proxyToAsType $ Proxy @(Hash BlockHeader)) bytes of
      Left err -> fail (show err)
      Right headerHash -> pure headerHash

deriving stock instance Generic ChainPoint

instance ToCBOR ChainPoint where
  toCBOR = genericToCBOR

instance FromCBOR ChainPoint where
  fromCBOR = genericFromCBOR
