{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.ServerOutput where

import Hydra.Ledger (Tx, Utxo, ValidationError)
import Hydra.Network (Host)
import Hydra.Party (Party)
import Hydra.Prelude
import Hydra.Snapshot (Snapshot)

data ServerOutput tx
  = PeerConnected {peer :: Host}
  | PeerDisconnected {peer :: Host}
  | ReadyToCommit {parties :: Set Party}
  | Committed {party :: Party, utxo :: Utxo tx}
  | HeadIsOpen {utxo :: Utxo tx}
  | HeadIsClosed {contestationDeadline :: UTCTime, latestSnapshot :: Snapshot tx}
  | HeadIsAborted {utxo :: Utxo tx}
  | HeadIsFinalized {utxo :: Utxo tx}
  | CommandFailed
  | TxSeen {transaction :: tx}
  | TxValid {transaction :: tx}
  | TxInvalid {utxo :: Utxo tx, transaction :: tx, validationError :: ValidationError}
  | SnapshotConfirmed {snapshot :: Snapshot tx}
  | Utxo {utxo :: Utxo tx}
  | InvalidInput {reason :: String, input :: Text}
  | -- | A friendly welcome message which tells a client something about the
    -- node. Currently used for knowing what signing key the server uses (it
    -- only knows one).
    Greetings {me :: Party}
  deriving (Generic)

deriving instance Tx tx => Eq (ServerOutput tx)
deriving instance Tx tx => Show (ServerOutput tx)
deriving instance Tx tx => ToJSON (ServerOutput tx)
deriving instance Tx tx => FromJSON (ServerOutput tx)

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
    TxInvalid u tx err -> TxInvalid <$> shrink u <*> shrink tx <*> shrink err
    SnapshotConfirmed s -> SnapshotConfirmed <$> shrink s
    Utxo u -> Utxo <$> shrink u
    InvalidInput r i -> InvalidInput <$> shrink r <*> shrink i
    Greetings me -> Greetings <$> shrink me
