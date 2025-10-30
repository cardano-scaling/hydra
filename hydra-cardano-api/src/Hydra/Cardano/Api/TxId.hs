{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.TxId where

import Hydra.Cardano.Api.Prelude

-- missing CBOR instances

instance ToCBOR TxId where
  toCBOR = toCBOR . serialiseToRawBytes

instance FromCBOR TxId where
  fromCBOR = do
    bs <- fromCBOR
    case deserialiseFromRawBytes AsTxId bs of
      Left err -> fail (show err)
      Right v -> pure v
