module Hydra.API.ServerOutputFilter where

import Hydra.API.ServerOutput (ServerOutput (..), TimedServerOutput, output)
import Hydra.Cardano.Api (
  Tx,
  serialiseToBech32,
  txOuts',
  pattern ShelleyAddressInEra,
  pattern TxOut,
 )
import Hydra.Prelude hiding (seq)
import Hydra.Tx (
  Snapshot (..),
 )

newtype ServerOutputFilter tx = ServerOutputFilter
  { txContainsAddr :: TimedServerOutput tx -> Text -> Bool
  }

serverOutputFilter :: ServerOutputFilter Tx
serverOutputFilter :: ServerOutputFilter Tx =
  ServerOutputFilter
    { txContainsAddr = \response address ->
        case output response of
          SnapshotConfirmed{snapshot = Snapshot{confirmed}} -> any (matchingAddr address) confirmed
          _ -> True
    }

matchingAddr :: Text -> Tx -> Bool
matchingAddr address tx =
  not . null $ flip filter (txOuts' tx) $ \(TxOut outAddr _ _ _) ->
    case outAddr of
      ShelleyAddressInEra addr -> serialiseToBech32 addr == address
      _ -> False
