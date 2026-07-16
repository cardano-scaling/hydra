{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ChainPoint where

import Hydra.Cardano.Api.Prelude

-- | Get the chain point corresponding to a given 'BlockHeader'.
getChainPoint :: BlockHeader -> ChainPoint
getChainPoint header =
  ChainPoint slotNo headerHash
 where
  (BlockHeader slotNo headerHash _) = header

-- * Orphans

-- missing CBOR instances

instance ToCBOR ChainPoint where
  toCBOR = \case
    ChainPointAtGenesis ->
      toCBOR ("ChainPointAtGenesis" :: Text)
    ChainPoint slotNo headerHash ->
      toCBOR ("ChainPoint" :: Text) <> toCBOR slotNo <> toCBOR (serialiseToRawBytes headerHash)

instance FromCBOR ChainPoint where
  fromCBOR =
    fromCBOR >>= \case
      ("ChainPointAtGenesis" :: Text) -> pure ChainPointAtGenesis
      "ChainPoint" -> do
        slotNo <- fromCBOR
        bytes <- fromCBOR
        case deserialiseFromRawBytes (proxyToAsType $ Proxy @(Hash BlockHeader)) bytes of
          Left err -> fail (show err)
          Right headerHash -> pure $ ChainPoint slotNo headerHash
      tag -> fail $ show (tag :: Text) <> " is not a proper CBOR-encoded ChainPoint"
