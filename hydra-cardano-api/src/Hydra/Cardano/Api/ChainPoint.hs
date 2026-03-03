{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ChainPoint where

import Hydra.Cardano.Api.Prelude

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Data.Aeson (eitherDecodeStrict', encode)
import Data.ByteString.Lazy qualified as BSL

-- | Get the chain point corresponding to a given 'BlockHeader'.
getChainPoint :: BlockHeader -> ChainPoint
getChainPoint header =
  ChainPoint slotNo headerHash
 where
  (BlockHeader slotNo headerHash _) = header

-- Orphan instances for serialization
-- We serialize ChainPoint via JSON encoding since it already has ToJSON/FromJSON instances
instance ToCBOR ChainPoint where
  toCBOR = toCBOR . BSL.toStrict . encode

instance FromCBOR ChainPoint where
  fromCBOR =
    fromCBOR >>= \bytes ->
      case eitherDecodeStrict' bytes of
        Left err -> fail $ "Failed to deserialize ChainPoint: " <> err
        Right chainPoint -> pure chainPoint
