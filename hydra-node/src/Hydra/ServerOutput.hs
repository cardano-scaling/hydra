{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.ServerOutput where

import Hydra.Chain (PostChainTx, PostTxError)
import Hydra.Ledger (IsTx, TxIdType, UtxoType, ValidationError)
import Hydra.Network (Host)
import Hydra.Party (MultiSigned, Party)
import Hydra.Prelude
import Hydra.Snapshot (Snapshot)

data ServerOutput tx
  = PeerConnected {peer :: Host}
  | PeerDisconnected {peer :: Host}
  | ReadyToCommit {parties :: Set Party}
  | Committed {party :: Party, utxo :: UtxoType tx}
  | HeadIsOpen {utxo :: UtxoType tx}
  | HeadIsClosed {contestationDeadline :: UTCTime, latestSnapshot :: Snapshot tx}
  | HeadIsAborted {utxo :: UtxoType tx}
  | HeadIsFinalized {utxo :: UtxoType tx}
  | CommandFailed
  | TxSeen {transaction :: tx}
  | TxValid {transaction :: tx}
  | TxInvalid {utxo :: UtxoType tx, transaction :: tx, validationError :: ValidationError}
  | SnapshotConfirmed {snapshot :: Snapshot tx, signatures :: MultiSigned (Snapshot tx)}
  | -- XXX(SN): This is too vague of a name and prone to conflict. Also we want
    -- to relate it to 'GetUtxo' from 'ClientInput', so 'GetUtxoResult' might be
    -- a better name
    Utxo {utxo :: UtxoType tx}
  | InvalidInput {reason :: String, input :: Text}
  | -- | A friendly welcome message which tells a client something about the
    -- node. Currently used for knowing what signing key the server uses (it
    -- only knows one).
    Greetings {me :: Party}
  | PostTxOnChainFailed {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
  deriving (Generic)

deriving instance IsTx tx => Eq (ServerOutput tx)
deriving instance IsTx tx => Show (ServerOutput tx)
deriving instance IsTx tx => ToJSON (ServerOutput tx)
deriving instance IsTx tx => FromJSON (ServerOutput tx)

instance (Arbitrary tx, Arbitrary (UtxoType tx), Arbitrary (TxIdType tx)) => Arbitrary (ServerOutput tx) where
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
    TxInvalid u tx err -> TxInvalid <$> shrink u <*> shrink tx <*> shrink err
    SnapshotConfirmed s ms -> SnapshotConfirmed <$> shrink s <*> shrink ms
    Utxo u -> Utxo <$> shrink u
    InvalidInput r i -> InvalidInput <$> shrink r <*> shrink i
    Greetings me -> Greetings <$> shrink me
    PostTxOnChainFailed p e -> PostTxOnChainFailed <$> shrink p <*> shrink e
