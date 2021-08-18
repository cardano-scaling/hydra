{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.ServerOutput where

import Data.Aeson (object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import Hydra.Ledger (Party, Tx, Utxo)
import Hydra.Prelude
import Hydra.Snapshot (Snapshot)

data ServerOutput tx
  = PeerConnected Party
  | PeerDisconnected Party
  | ReadyToCommit [Party]
  | Committed Party (Utxo tx)
  | HeadIsOpen (Utxo tx)
  | HeadIsClosed DiffTime (Snapshot tx)
  | HeadIsAborted (Utxo tx)
  | HeadIsFinalized (Utxo tx)
  | CommandFailed
  | TxSeen tx
  | TxValid tx
  | TxInvalid tx
  | SnapshotConfirmed (Snapshot tx)
  | Utxo (Utxo tx)
  | InvalidInput
  deriving (Generic)

deriving instance Tx tx => Eq (ServerOutput tx)
deriving instance Tx tx => Show (ServerOutput tx)

instance (Arbitrary tx, Arbitrary (Utxo tx)) => Arbitrary (ServerOutput tx) where
  arbitrary = genericArbitrary

  -- NOTE: See note on 'Arbitrary (ClientInput tx)'
  shrink = \case
    PeerConnected p -> PeerConnected <$> shrink p
    PeerDisconnected p -> PeerDisconnected <$> shrink p
    ReadyToCommit xs -> ReadyToCommit <$> shrink xs
    Committed p u -> Committed <$> shrink p <*> shrink u
    HeadIsOpen u -> HeadIsOpen <$> shrink u
    HeadIsClosed t s -> HeadIsClosed t <$> shrink s
    HeadIsFinalized u -> HeadIsFinalized <$> shrink u
    HeadIsAborted u -> HeadIsAborted <$> shrink u
    CommandFailed -> []
    TxSeen tx -> TxSeen <$> shrink tx
    TxValid tx -> TxValid <$> shrink tx
    TxInvalid tx -> TxInvalid <$> shrink tx
    SnapshotConfirmed{} -> []
    Utxo u -> Utxo <$> shrink u
    InvalidInput -> []

instance (ToJSON tx, ToJSON (Snapshot tx), ToJSON (Utxo tx)) => ToJSON (ServerOutput tx) where
  toJSON = \case
    PeerConnected peer ->
      object [tagFieldName .= s "peerConnected", "peer" .= peer]
    PeerDisconnected peer ->
      object [tagFieldName .= s "peerDisconnected", "peer" .= peer]
    ReadyToCommit parties ->
      object [tagFieldName .= s "readyToCommit", "parties" .= parties]
    Committed party utxo ->
      object [tagFieldName .= s "committed", "party" .= party, "utxo" .= utxo]
    HeadIsOpen utxo ->
      object [tagFieldName .= s "headIsOpen", "utxo" .= utxo]
    HeadIsClosed contestationPeriod latestSnapshot ->
      object
        [ tagFieldName .= s "headIsClosed"
        , "contestationPeriod" .= contestationPeriod
        , "latestSnapshot" .= latestSnapshot
        ]
    HeadIsFinalized utxo ->
      object [tagFieldName .= s "headIsFinalized", "utxo" .= utxo]
    HeadIsAborted utxo ->
      object [tagFieldName .= s "headIsAborted", "utxo" .= utxo]
    CommandFailed ->
      object [tagFieldName .= s "commandFailed"]
    TxSeen tx ->
      object [tagFieldName .= s "transactionSeen", "transaction" .= tx]
    TxValid tx ->
      object [tagFieldName .= s "transactionValid", "transaction" .= tx]
    TxInvalid tx ->
      object [tagFieldName .= s "transactionInvalid", "transaction" .= tx]
    SnapshotConfirmed snapshotNumber ->
      object [tagFieldName .= s "snapshotConfirmed", "snapshot" .= snapshotNumber]
    Utxo utxo ->
      object [tagFieldName .= s "utxo", "utxo" .= utxo]
    InvalidInput ->
      object [tagFieldName .= s "invalidInput"]
   where
    s = Aeson.String
    tagFieldName = "output"

instance (FromJSON tx, FromJSON (Snapshot tx), FromJSON (Utxo tx)) => FromJSON (ServerOutput tx) where
  parseJSON = withObject "ServerOutput" $ \obj -> do
    tag <- obj .: "output"
    case tag of
      "peerConnected" ->
        PeerConnected <$> (obj .: "peer")
      "peerDisconnected" ->
        PeerDisconnected <$> (obj .: "peer")
      "readyToCommit" ->
        ReadyToCommit <$> (obj .: "parties")
      "committed" ->
        Committed <$> (obj .: "party") <*> (obj .: "utxo")
      "headIsOpen" ->
        HeadIsOpen <$> (obj .: "utxo")
      "headIsClosed" ->
        HeadIsClosed <$> (obj .: "contestationPeriod") <*> (obj .: "latestSnapshot")
      "headIsFinalized" ->
        HeadIsFinalized <$> (obj .: "utxo")
      "headIsAborted" ->
        HeadIsAborted <$> (obj .: "utxo")
      "commandFailed" ->
        pure CommandFailed
      "transactionSeen" ->
        TxSeen <$> (obj .: "transaction")
      "transactionValid" ->
        TxValid <$> (obj .: "transaction")
      "transactionInvalid" ->
        TxInvalid <$> (obj .: "transaction")
      "snapshotConfirmed" ->
        SnapshotConfirmed <$> (obj .: "snapshot")
      "utxo" ->
        Utxo <$> (obj .: "utxo")
      "invalidInput" ->
        pure InvalidInput
      _ ->
        fail $ "unknown output type: " <> toString @Text tag
