module Hydra.API.ServerOutputFilter where

import Hydra.API.ServerOutput (ServerOutput (..), TimedServerOutput, output)
import Hydra.Cardano.Api (
  AddressInEra,
  Tx,
  serialiseToBech32,
  txOuts',
  pattern ByronAddressInEra,
  pattern ShelleyAddressInEra,
  pattern TxOut,
 )
import Hydra.Prelude hiding (seq)

newtype ServerOutputFilter tx = ServerOutputFilter
  { txContainsAddr :: TimedServerOutput tx -> Text -> Bool
  }

serverOutputFilter :: ServerOutputFilter Tx
serverOutputFilter :: ServerOutputFilter Tx =
  ServerOutputFilter
    { txContainsAddr = \response address ->
        case output response of
          TxValid{transaction} -> matchingAddr address transaction
          TxInvalid{transaction} -> matchingAddr address transaction
          _ -> True
    }

matchingAddr :: Text -> Tx -> Bool
matchingAddr address tx =
  not . null $ flip filter (txOuts' tx) $ \(TxOut addr _ _ _) ->
    unwrapAddress addr == address

unwrapAddress :: AddressInEra -> Text
unwrapAddress = \case
  ShelleyAddressInEra addr -> serialiseToBech32 addr
  ByronAddressInEra{} -> error "Byron."
