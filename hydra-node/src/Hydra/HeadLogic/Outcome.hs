{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic.Outcome where

import Hydra.Prelude

import Hydra.API.ServerOutput (ClientMessage, DecommitInvalidReason)
import Hydra.Chain (PostChainTx)
import Hydra.Chain.ChainState (ChainPointType, ChainSlot, ChainStateType, IsChainState)
import Hydra.HeadLogic.Error (LogicError)
import Hydra.Ledger (ValidationError)
import Hydra.Network (Host, ProtocolVersion)
import Hydra.Network.Message (Message)
import Hydra.Node.State (Deposit, NodeState)
import Hydra.Tx (
  HeadId,
  HeadParameters,
  HeadSeed,
  IsTx,
  Party,
  Snapshot,
  SnapshotNumber,
  SnapshotVersion,
  TxIdType,
  UTxOType,
 )
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Crypto (MultiSignature, Signature)
import Hydra.Tx.OnChainId (OnChainId)

-- | Analogous to inputs, the pure head logic "core" can have effects emitted to
-- the "shell" layers and we distinguish the same: effects onto the client, the
-- network and the chain.
data Effect tx
  = -- | Effect to be handled by the "Hydra.API", results in sending this 'ClientMessage'.
    ClientEffect {clientMessage :: ClientMessage tx}
  | -- | Effect to be handled by a "Hydra.Network", results in a 'Hydra.Network.broadcast'.
    NetworkEffect {message :: Message tx}
  | -- | Effect to be handled by a "Hydra.Chain", results in a 'Hydra.Chain.postTx'.
    OnChainEffect {postChainTx :: PostChainTx tx}
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (Effect tx)
deriving stock instance IsChainState tx => Show (Effect tx)
deriving anyclass instance IsChainState tx => ToJSON (Effect tx)
deriving anyclass instance IsChainState tx => FromJSON (Effect tx)

-- | Head state changed event. These events represent all the internal state
-- changes, get persisted and processed in an event sourcing manner.
data StateChanged tx
  = NetworkConnected
  | NetworkDisconnected
  | PeerConnected {peer :: Host}
  | PeerDisconnected {peer :: Host}
  | NetworkVersionMismatch
      { ourVersion :: ProtocolVersion
      , theirVersion :: Maybe ProtocolVersion
      }
  | NetworkClusterIDMismatch
      { clusterPeers :: Text
      , misconfiguredPeers :: Text
      }
  | HeadInitialized
      { parameters :: HeadParameters
      , chainState :: ChainStateType tx
      , headId :: HeadId
      , headSeed :: HeadSeed
      , parties :: [Party]
      }
  | CommittedUTxO
      { headId :: HeadId
      , party :: Party
      , committedUTxO :: UTxOType tx
      , chainState :: ChainStateType tx
      }
  | HeadAborted {headId :: HeadId, utxo :: UTxOType tx, chainState :: ChainStateType tx}
  | HeadOpened {headId :: HeadId, chainState :: ChainStateType tx, initialUTxO :: UTxOType tx}
  | TransactionReceived {tx :: tx}
  | TransactionAppliedToLocalUTxO
      { headId :: HeadId
      , tx :: tx
      , newLocalUTxO :: UTxOType tx
      }
  | SnapshotRequestDecided {snapshotNumber :: SnapshotNumber}
  | -- | A snapshot was requested by some party.
    -- NOTE: We deliberately already include an updated local ledger state to
    -- not need a ledger to interpret this event.
    SnapshotRequested
      { snapshot :: Snapshot tx
      , requestedTxIds :: [TxIdType tx]
      , newLocalUTxO :: UTxOType tx
      , newLocalTxs :: [tx]
      , newCurrentDepositTxId :: Maybe (TxIdType tx)
      }
  | PartySignedSnapshot {snapshot :: Snapshot tx, party :: Party, signature :: Signature (Snapshot tx)}
  | SnapshotConfirmed {headId :: HeadId, snapshot :: Snapshot tx, signatures :: MultiSignature (Snapshot tx)}
  | DepositRecorded
      { chainState :: ChainStateType tx
      , headId :: HeadId
      , depositTxId :: TxIdType tx
      , deposited :: UTxOType tx
      , created :: UTCTime
      , deadline :: UTCTime
      }
  | DepositActivated {depositTxId :: TxIdType tx, chainTime :: UTCTime, deposit :: Deposit tx}
  | DepositExpired {depositTxId :: TxIdType tx, chainTime :: UTCTime, deposit :: Deposit tx}
  | DepositRecovered
      { chainState :: ChainStateType tx
      , headId :: HeadId
      , depositTxId :: TxIdType tx
      , recovered :: UTxOType tx
      }
  | CommitApproved {headId :: HeadId, utxoToCommit :: UTxOType tx}
  | CommitFinalized
      { chainState :: ChainStateType tx
      , headId :: HeadId
      , newVersion :: SnapshotVersion
      , depositTxId :: TxIdType tx
      }
  | DecommitRecorded {headId :: HeadId, decommitTx :: tx, newLocalUTxO :: UTxOType tx, utxoToDecommit :: UTxOType tx}
  | DecommitApproved {headId :: HeadId, decommitTxId :: TxIdType tx, utxoToDecommit :: UTxOType tx}
  | DecommitInvalid {headId :: HeadId, decommitTx :: tx, decommitInvalidReason :: DecommitInvalidReason tx}
  | DecommitFinalized
      { chainState :: ChainStateType tx
      , headId :: HeadId
      , distributedUTxO :: UTxOType tx
      , newVersion :: SnapshotVersion
      }
  | HeadClosed {headId :: HeadId, snapshotNumber :: SnapshotNumber, chainState :: ChainStateType tx, contestationDeadline :: UTCTime}
  | HeadContested {headId :: HeadId, chainState :: ChainStateType tx, contestationDeadline :: UTCTime, snapshotNumber :: SnapshotNumber}
  | HeadIsReadyToFanout {headId :: HeadId}
  | HeadFannedOut {headId :: HeadId, utxo :: UTxOType tx, chainState :: ChainStateType tx}
  | ChainRolledBack {chainState :: ChainStateType tx}
  | TickObserved {chainPoint :: ChainPointType tx}
  | IgnoredHeadInitializing
      { headId :: HeadId
      , contestationPeriod :: ContestationPeriod
      , parties :: [Party]
      , participants :: [OnChainId]
      }
  | TxInvalid {headId :: HeadId, utxo :: UTxOType tx, transaction :: tx, validationError :: ValidationError}
  | LocalStateCleared {headId :: HeadId, snapshotNumber :: SnapshotNumber}
  | Checkpoint {state :: NodeState tx}
  | NodeUnsynced {chainPoint :: ChainPointType tx}
  | NodeSynced {chainPoint :: ChainPointType tx}
  deriving stock (Generic)

deriving stock instance (IsChainState tx, IsTx tx, Eq (NodeState tx), Eq (ChainStateType tx)) => Eq (StateChanged tx)
deriving stock instance (IsChainState tx, IsTx tx, Show (NodeState tx), Show (ChainStateType tx)) => Show (StateChanged tx)
deriving anyclass instance (IsChainState tx, IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (StateChanged tx)
deriving anyclass instance (IsChainState tx, IsTx tx, FromJSON (NodeState tx), FromJSON (ChainStateType tx)) => FromJSON (StateChanged tx)

data Outcome tx
  = -- | Continue with the given state updates and side effects.
    Continue {stateChanges :: [StateChanged tx], effects :: [Effect tx]}
  | -- | Wait for some condition to be met with optional state updates.
    Wait {reason :: WaitReason tx, stateChanges :: [StateChanged tx]}
  | -- | Processing resulted in an error.
    Error {error :: LogicError tx}
  deriving stock (Generic)

instance Semigroup (Outcome tx) where
  e@Error{} <> _ = e
  _ <> e@Error{} = e
  Continue scA _ <> Wait r scB = Wait r (scA <> scB)
  Wait r scA <> _ = Wait r scA
  Continue scA efA <> Continue scB efB = Continue (scA <> scB) (efA <> efB)

deriving stock instance IsChainState tx => Eq (Outcome tx)
deriving stock instance IsChainState tx => Show (Outcome tx)
deriving anyclass instance IsChainState tx => ToJSON (Outcome tx)
deriving anyclass instance IsChainState tx => FromJSON (Outcome tx)

noop :: Outcome tx
noop = Continue [] []

wait :: WaitReason tx -> Outcome tx
wait reason = Wait reason []

newState :: StateChanged tx -> Outcome tx
newState change = Continue [change] []

cause :: Effect tx -> Outcome tx
cause e = Continue [] [e]

causes :: [Effect tx] -> Outcome tx
causes = Continue []

changes :: [StateChanged tx] -> Outcome tx
changes stateChanges = Continue stateChanges []

data WaitReason tx
  = WaitOnNotApplicableTx {validationError :: ValidationError}
  | WaitOnSnapshotNumber {waitingForNumber :: SnapshotNumber}
  | WaitOnSnapshotVersion {waitingForVersion :: SnapshotVersion}
  | WaitOnSeenSnapshot
  | WaitOnTxs {waitingForTxIds :: [TxIdType tx]}
  | WaitOnContestationDeadline
  | WaitOnNotApplicableDecommitTx {notApplicableReason :: DecommitInvalidReason tx}
  | WaitOnUnresolvedCommit {commitUTxO :: UTxOType tx}
  | WaitOnUnresolvedDecommit {decommitTx :: tx}
  | WaitOnDepositObserved {depositTxId :: TxIdType tx}
  | WaitOnDepositActivation {depositTxId :: TxIdType tx}
  | WaitOnNodeInSync {currentSlot :: ChainSlot}
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (WaitReason tx)
deriving stock instance IsTx tx => Show (WaitReason tx)
deriving anyclass instance IsTx tx => ToJSON (WaitReason tx)
deriving anyclass instance IsTx tx => FromJSON (WaitReason tx)
