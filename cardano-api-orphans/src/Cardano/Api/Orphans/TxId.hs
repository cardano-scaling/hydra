module Cardano.Api.Orphans.TxId where

import Cardano.Api (AsType (AsTxId), TxId, deserialiseFromRawBytes, serialiseToRawBytes)
import Cardano.Binary (FromCBOR (..), ToCBOR (..))

instance ToCBOR TxId where
  toCBOR = toCBOR . serialiseToRawBytes

instance FromCBOR TxId where
  fromCBOR = do
    bs <- fromCBOR
    case deserialiseFromRawBytes AsTxId bs of
      Left err -> fail (show err)
      Right v -> pure v
