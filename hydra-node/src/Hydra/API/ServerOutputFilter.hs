module Hydra.API.ServerOutputFilter where

import Hydra.API.ServerOutput (ServerOutput (..), TimedServerOutput, output)
import Hydra.Cardano.Api (
  AddressInEra,
  Tx,
  deserialiseAddress,
  isSignedByAddress,
  proxyToAsType,
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
          SnapshotConfirmed{snapshot = Snapshot{confirmed}} ->
            -- A snapshot can confirm no transaction at all, settling only a
            -- deposit or a decommit. There is then no address to match
            -- against, so it reaches every client rather than none: this
            -- filter narrows what a client sees, it does not withhold
            -- events that carry no address.
            null confirmed || any (matchingAddr address) confirmed
          _ -> True
    }

-- | Whether a transaction involves the given address, by paying to it or by
-- spending from it.
--
-- Both directions matter to a client watching its own address: the snapshot in
-- which its funds leave the head is the one it most needs, and a transaction
-- that spends a UTxO in full leaves no output behind to match on.
matchingAddr :: Text -> Tx -> Bool
matchingAddr address tx =
  paysToAddress || spendsFromAddress
 where
  paysToAddress =
    not . null $ flip filter (txOuts' tx) $ \(TxOut outAddr _ _ _) ->
      case outAddr of
        ShelleyAddressInEra addr -> serialiseToBech32 addr == address
        _ -> False

  -- Spending is matched on the payment credential rather than the serialised
  -- address, since a transaction's inputs do not carry the address they were
  -- locked to.
  spendsFromAddress =
    maybe False (`isSignedByAddress` tx) parsedAddress

  parsedAddress =
    deserialiseAddress (proxyToAsType (Proxy @AddressInEra)) address
