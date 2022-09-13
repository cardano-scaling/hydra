{-# LANGUAGE UndecidableInstances #-}

module Hydra.ServerOutput where

import Hydra.Chain (PostChainTx, PostTxError)
import Hydra.ClientInput (ClientInput (..))
import Hydra.Crypto (MultiSignature)
import Hydra.Ledger (IsTx, UTxOType, ValidationError)
import Hydra.Network (Host)
import Hydra.Party (Party)
import Hydra.Prelude
import Hydra.Snapshot (Snapshot, SnapshotNumber)

data ServerOutput tx
  = PeerConnected {peer :: Host}
  | PeerDisconnected {peer :: Host}
  | ReadyToCommit {parties :: Set Party}
  | Committed {party :: Party, utxo :: UTxOType tx}
  | HeadIsOpen {utxo :: UTxOType tx}
  | HeadIsClosed
      { snapshotNumber :: SnapshotNumber
      , -- | Nominal deadline until which contest can be submitted and after
        -- which fanout is possible. NOTE: Use this only for informational
        -- purpose and wait for 'ReadyToFanout' instead before sending 'Fanout'
        -- as the ledger of our cardano-node might not have progressed
        -- sufficiently in time yet and we do not re-submit transactions (yet).
        contestationDeadline :: UTCTime
      }
  | HeadIsContested {snapshotNumber :: SnapshotNumber}
  | ReadyToFanout
  | HeadIsAborted {utxo :: UTxOType tx}
  | HeadIsFinalized {utxo :: UTxOType tx}
  | CommandFailed {clientInput :: ClientInput tx}
  | TxSeen {transaction :: tx}
  | TxValid {transaction :: tx}
  | TxInvalid {utxo :: UTxOType tx, transaction :: tx, validationError :: ValidationError}
  | SnapshotConfirmed
      { snapshot :: Snapshot tx
      , signatures :: MultiSignature (Snapshot tx)
      }
  | GetUTxOResponse {utxo :: UTxOType tx}
  | InvalidInput {reason :: String, input :: Text}
  | -- | A friendly welcome message which tells a client something about the
    -- node. Currently used for knowing what signing key the server uses (it
    -- only knows one).
    Greetings {me :: Party}
  | PostTxOnChainFailed {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
  | RolledBack
  deriving (Generic)

deriving instance IsTx tx => Eq (ServerOutput tx)
deriving instance IsTx tx => Show (ServerOutput tx)
deriving instance IsTx tx => ToJSON (ServerOutput tx)
deriving instance IsTx tx => FromJSON (ServerOutput tx)

instance IsTx tx => Arbitrary (ServerOutput tx) where
  arbitrary = genericArbitrary

  -- NOTE: See note on 'Arbitrary (ClientInput tx)'
  shrink = \case
    PeerConnected p -> PeerConnected <$> shrink p
    PeerDisconnected p -> PeerDisconnected <$> shrink p
    ReadyToCommit xs -> ReadyToCommit <$> shrink xs
    Committed p u -> Committed <$> shrink p <*> shrink u
    HeadIsOpen u -> HeadIsOpen <$> shrink u
    HeadIsClosed s t -> HeadIsClosed <$> shrink s <*> shrink t
    HeadIsContested sn -> HeadIsContested <$> shrink sn
    ReadyToFanout -> []
    HeadIsFinalized u -> HeadIsFinalized <$> shrink u
    HeadIsAborted u -> HeadIsAborted <$> shrink u
    CommandFailed i -> CommandFailed <$> shrink i
    TxSeen tx -> TxSeen <$> shrink tx
    TxValid tx -> TxValid <$> shrink tx
    TxInvalid u tx err -> TxInvalid <$> shrink u <*> shrink tx <*> shrink err
    SnapshotConfirmed s ms -> SnapshotConfirmed <$> shrink s <*> shrink ms
    GetUTxOResponse u -> GetUTxOResponse <$> shrink u
    InvalidInput r i -> InvalidInput <$> shrink r <*> shrink i
    Greetings me -> Greetings <$> shrink me
    PostTxOnChainFailed p e -> PostTxOnChainFailed <$> shrink p <*> shrink e
    RolledBack -> []
