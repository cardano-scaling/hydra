{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic.Outcome where

import Hydra.Prelude

import Data.Aeson (Value (..), defaultOptions, genericParseJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Hydra.API.ServerOutput (ClientMessage, DecommitInvalidReason)
import Hydra.Chain (PostChainTx)
import Hydra.Chain.ChainState (ChainPointType, ChainSlot, ChainStateType, IsChainState)
import Hydra.HeadLogic.Error (LogicError)
import Hydra.HeadLogic.State (FanoutMode (..))
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
  | HeadOpened
      { parameters :: HeadParameters
      , chainState :: ChainStateType tx
      , headId :: HeadId
      , headSeed :: HeadSeed
      , parties :: [Party]
      }
  | TransactionReceived {tx :: tx}
  | TransactionAppliedToLocalUTxO
      { headId :: HeadId
      , tx :: tx
      }
  | SnapshotRequestDecided {snapshotNumber :: SnapshotNumber}
  | SnapshotRequested
      { requestedSnapshot :: Snapshot tx
      , newLocalTxs :: Seq tx
      , newCurrentDepositTxId :: Maybe (TxIdType tx)
      }
  | PartySignedSnapshot {snapshotNumber :: SnapshotNumber, party :: Party, signature :: Signature (Snapshot tx)}
  | SnapshotConfirmed
      { headId :: HeadId
      , snapshot :: Maybe (Snapshot tx)
      -- ^ 'Nothing' on the normal signing path (snapshot already in 'seenSnapshot');
      -- 'Just' only for the side-load path where no preceding 'SnapshotRequested' exists.
      , signatures :: MultiSignature (Snapshot tx)
      }
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
  | -- TODO: Rename to DepositApproved
    CommitApproved {headId :: HeadId, utxoToCommit :: UTxOType tx}
  | -- TODO: Rename to DepositFinalized
    CommitFinalized
      { chainState :: ChainStateType tx
      , headId :: HeadId
      , newVersion :: SnapshotVersion
      , depositTxId :: TxIdType tx
      }
  | DecommitRecorded {headId :: HeadId, decommitTx :: tx}
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
  | -- | This node initiated a full automatic fanout ('Fanout' command). It
    -- transitions 'Closed' → 'PartialFanout' in 'AutoDrain' mode so that this
    -- node (the driver) auto-continues draining as chunks are observed. Other
    -- parties that merely observe the resulting partial fanout do NOT auto-drive
    -- (they go to 'AwaitingSelection'). No chain state change (client command).
    HeadFanoutInitiated
      { headId :: HeadId
      , remainingOutputs :: UTxOType tx
      }
  | -- | A user initiated or updated a selective partial fanout. From 'Closed'
    -- this transitions into the 'PartialFanout' state (using 'remainingOutputs'
    -- as the initial remaining set); from 'PartialFanout' it just updates the
    -- active selection. No chain state change (driven by a client command).
    HeadPartialFanoutSelected
      { headId :: HeadId
      , remainingOutputs :: UTxOType tx
      , selection :: UTxOType tx
      }
  | -- | Revert an optimistic 'Closed' → 'PartialFanout' transition back to
    -- 'Closed'. Emitted when posting the initiating fanout transaction fails
    -- terminally /before/ any partial fanout has landed on chain (i.e. while the
    -- off-chain state is 'PartialFanout' but the on-chain datum is still
    -- 'Closed'). Without this the head would wedge in 'PartialFanout' with
    -- 'Fanout' rejected. No chain state change.
    HeadFanoutReverted {headId :: HeadId}
  | HeadFannedOut {headId :: HeadId, finalizedOutputs :: UTxOType tx, chainState :: ChainStateType tx}
  | HeadPartialFannedOut
      { headId :: HeadId
      , distributedOutputs :: UTxOType tx
      , remainingOutputs :: UTxOType tx
      , chainState :: ChainStateType tx
      , mode :: FanoutMode tx
      -- ^ The fanout mode to continue with after this step (drives whether the
      --   node auto-resumes draining or waits for the next 'PartialFanout').
      }
  | ChainRolledBack {chainState :: ChainStateType tx}
  | TickObserved {chainPoint :: ChainPointType tx, chainTime :: UTCTime}
  | IgnoredHeadInitializing
      { headId :: HeadId
      , contestationPeriod :: ContestationPeriod
      , parties :: [Party]
      , participants :: [OnChainId]
      }
  | TxInvalid {headId :: HeadId, utxo :: UTxOType tx, transaction :: tx, validationError :: ValidationError}
  | LocalStateCleared {headId :: HeadId, snapshotNumber :: SnapshotNumber}
  | Checkpoint {state :: NodeState tx}
  | NodeUnsynced {chainSlot :: ChainSlot, chainTime :: UTCTime, drift :: NominalDiffTime}
  | NodeSynced {chainSlot :: ChainSlot, chainTime :: UTCTime, drift :: NominalDiffTime}
  deriving stock (Generic)

deriving stock instance (IsChainState tx, IsTx tx, Eq (NodeState tx), Eq (ChainStateType tx)) => Eq (StateChanged tx)
deriving stock instance (IsChainState tx, IsTx tx, Show (NodeState tx), Show (ChainStateType tx)) => Show (StateChanged tx)
deriving anyclass instance (IsChainState tx, IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (StateChanged tx)

-- | Decoded generically, except that a 'HeadPartialFannedOut' persisted before
-- the 'mode' field existed (event logs from an earlier node) is decoded with
-- 'mode' defaulting to 'AwaitingSelection'. That is the safe default: the node
-- waits for the next 'PartialFanout' rather than assuming an auto-drain, so a
-- node mid-fanout can still restart. All other constructors decode as before.
instance forall tx. (IsChainState tx, IsTx tx, FromJSON (NodeState tx), FromJSON (ChainStateType tx)) => FromJSON (StateChanged tx) where
  parseJSON = genericParseJSON defaultOptions . withDefaultFanoutMode
   where
    withDefaultFanoutMode = \case
      Object o
        | Just (String "HeadPartialFannedOut") <- KeyMap.lookup "tag" o
        , not (KeyMap.member "mode" o) ->
            Object (KeyMap.insert "mode" (toJSON (AwaitingSelection :: FanoutMode tx)) o)
      v -> v

-- NOTE: This codec defines the event format persisted in the hydra.db events
-- table (see "Hydra.Events.SQLiteBased"). Changing an encoding here breaks
-- decoding of existing databases and requires a schema migration.
instance IsChainState tx => ToCBOR (StateChanged tx) where
  toCBOR = \case
    NetworkConnected ->
      toCBOR ("NetworkConnected" :: Text)
    NetworkDisconnected ->
      toCBOR ("NetworkDisconnected" :: Text)
    PeerConnected{peer} ->
      toCBOR ("PeerConnected" :: Text) <> toCBOR peer
    PeerDisconnected{peer} ->
      toCBOR ("PeerDisconnected" :: Text) <> toCBOR peer
    NetworkVersionMismatch{ourVersion, theirVersion} ->
      toCBOR ("NetworkVersionMismatch" :: Text) <> toCBOR ourVersion <> toCBOR theirVersion
    NetworkClusterIDMismatch{clusterPeers, misconfiguredPeers} ->
      toCBOR ("NetworkClusterIDMismatch" :: Text) <> toCBOR clusterPeers <> toCBOR misconfiguredPeers
    HeadOpened{parameters, chainState, headId, headSeed, parties} ->
      toCBOR ("HeadOpened" :: Text)
        <> toCBOR parameters
        <> toCBOR chainState
        <> toCBOR headId
        <> toCBOR headSeed
        <> toCBOR parties
    TransactionReceived{tx} ->
      toCBOR ("TransactionReceived" :: Text) <> toCBOR tx
    TransactionAppliedToLocalUTxO{headId, tx} ->
      toCBOR ("TransactionAppliedToLocalUTxO" :: Text) <> toCBOR headId <> toCBOR tx
    SnapshotRequestDecided{snapshotNumber} ->
      toCBOR ("SnapshotRequestDecided" :: Text) <> toCBOR snapshotNumber
    SnapshotRequested{requestedSnapshot, newLocalTxs, newCurrentDepositTxId} ->
      toCBOR ("SnapshotRequested" :: Text)
        <> toCBOR requestedSnapshot
        <> toCBOR newLocalTxs
        <> toCBOR newCurrentDepositTxId
    PartySignedSnapshot{snapshotNumber, party, signature} ->
      toCBOR ("PartySignedSnapshot" :: Text)
        <> toCBOR snapshotNumber
        <> toCBOR party
        <> toCBOR signature
    SnapshotConfirmed{headId, snapshot, signatures} ->
      toCBOR ("SnapshotConfirmed" :: Text)
        <> toCBOR headId
        <> toCBOR snapshot
        <> toCBOR signatures
    DepositRecorded{chainState, headId, depositTxId, deposited, created, deadline} ->
      toCBOR ("DepositRecorded" :: Text)
        <> toCBOR chainState
        <> toCBOR headId
        <> toCBOR depositTxId
        <> toCBOR deposited
        <> toCBOR created
        <> toCBOR deadline
    DepositActivated{depositTxId, chainTime, deposit} ->
      toCBOR ("DepositActivated" :: Text)
        <> toCBOR depositTxId
        <> toCBOR chainTime
        <> toCBOR deposit
    DepositExpired{depositTxId, chainTime, deposit} ->
      toCBOR ("DepositExpired" :: Text)
        <> toCBOR depositTxId
        <> toCBOR chainTime
        <> toCBOR deposit
    DepositRecovered{chainState, headId, depositTxId, recovered} ->
      toCBOR ("DepositRecovered" :: Text)
        <> toCBOR chainState
        <> toCBOR headId
        <> toCBOR depositTxId
        <> toCBOR recovered
    CommitApproved{headId, utxoToCommit} ->
      toCBOR ("CommitApproved" :: Text) <> toCBOR headId <> toCBOR utxoToCommit
    CommitFinalized{chainState, headId, newVersion, depositTxId} ->
      toCBOR ("CommitFinalized" :: Text)
        <> toCBOR chainState
        <> toCBOR headId
        <> toCBOR newVersion
        <> toCBOR depositTxId
    DecommitRecorded{headId, decommitTx} ->
      toCBOR ("DecommitRecorded" :: Text) <> toCBOR headId <> toCBOR decommitTx
    DecommitApproved{headId, decommitTxId, utxoToDecommit} ->
      toCBOR ("DecommitApproved" :: Text)
        <> toCBOR headId
        <> toCBOR decommitTxId
        <> toCBOR utxoToDecommit
    DecommitInvalid{headId, decommitTx, decommitInvalidReason} ->
      toCBOR ("DecommitInvalid" :: Text)
        <> toCBOR headId
        <> toCBOR decommitTx
        <> toCBOR decommitInvalidReason
    DecommitFinalized{chainState, headId, distributedUTxO, newVersion} ->
      toCBOR ("DecommitFinalized" :: Text)
        <> toCBOR chainState
        <> toCBOR headId
        <> toCBOR distributedUTxO
        <> toCBOR newVersion
    HeadClosed{headId, snapshotNumber, chainState, contestationDeadline} ->
      toCBOR ("HeadClosed" :: Text)
        <> toCBOR headId
        <> toCBOR snapshotNumber
        <> toCBOR chainState
        <> toCBOR contestationDeadline
    HeadContested{headId, chainState, contestationDeadline, snapshotNumber} ->
      toCBOR ("HeadContested" :: Text)
        <> toCBOR headId
        <> toCBOR chainState
        <> toCBOR contestationDeadline
        <> toCBOR snapshotNumber
    HeadIsReadyToFanout{headId} ->
      toCBOR ("HeadIsReadyToFanout" :: Text) <> toCBOR headId
    HeadFanoutInitiated{headId, remainingOutputs} ->
      toCBOR ("HeadFanoutInitiated" :: Text)
        <> toCBOR headId
        <> toCBOR remainingOutputs
    HeadPartialFanoutSelected{headId, remainingOutputs, selection} ->
      toCBOR ("HeadPartialFanoutSelected" :: Text)
        <> toCBOR headId
        <> toCBOR remainingOutputs
        <> toCBOR selection
    HeadFanoutReverted{headId} ->
      toCBOR ("HeadFanoutReverted" :: Text) <> toCBOR headId
    HeadFannedOut{headId, finalizedOutputs, chainState} ->
      toCBOR ("HeadFannedOut" :: Text)
        <> toCBOR headId
        <> toCBOR finalizedOutputs
        <> toCBOR chainState
    HeadPartialFannedOut{headId, distributedOutputs, remainingOutputs, chainState, mode} ->
      toCBOR ("HeadPartialFannedOut" :: Text)
        <> toCBOR headId
        <> toCBOR distributedOutputs
        <> toCBOR remainingOutputs
        <> toCBOR chainState
        <> toCBOR mode
    ChainRolledBack{chainState} ->
      toCBOR ("ChainRolledBack" :: Text) <> toCBOR chainState
    TickObserved{chainPoint, chainTime} ->
      toCBOR ("TickObserved" :: Text) <> toCBOR chainPoint <> toCBOR chainTime
    IgnoredHeadInitializing{headId, contestationPeriod, parties, participants} ->
      toCBOR ("IgnoredHeadInitializing" :: Text)
        <> toCBOR headId
        <> toCBOR contestationPeriod
        <> toCBOR parties
        <> toCBOR participants
    TxInvalid{headId, utxo, transaction, validationError} ->
      toCBOR ("TxInvalid" :: Text)
        <> toCBOR headId
        <> toCBOR utxo
        <> toCBOR transaction
        <> toCBOR validationError
    LocalStateCleared{headId, snapshotNumber} ->
      toCBOR ("LocalStateCleared" :: Text) <> toCBOR headId <> toCBOR snapshotNumber
    Checkpoint{state = nodeState} ->
      toCBOR ("Checkpoint" :: Text) <> toCBOR nodeState
    NodeUnsynced{chainSlot, chainTime, drift} ->
      toCBOR ("NodeUnsynced" :: Text)
        <> toCBOR chainSlot
        <> toCBOR chainTime
        <> toCBOR drift
    NodeSynced{chainSlot, chainTime, drift} ->
      toCBOR ("NodeSynced" :: Text)
        <> toCBOR chainSlot
        <> toCBOR chainTime
        <> toCBOR drift

instance IsChainState tx => FromCBOR (StateChanged tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("NetworkConnected" :: Text) -> pure NetworkConnected
      "NetworkDisconnected" -> pure NetworkDisconnected
      "PeerConnected" -> PeerConnected <$> fromCBOR
      "PeerDisconnected" -> PeerDisconnected <$> fromCBOR
      "NetworkVersionMismatch" -> NetworkVersionMismatch <$> fromCBOR <*> fromCBOR
      "NetworkClusterIDMismatch" -> NetworkClusterIDMismatch <$> fromCBOR <*> fromCBOR
      "HeadOpened" -> HeadOpened <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "TransactionReceived" -> TransactionReceived <$> fromCBOR
      "TransactionAppliedToLocalUTxO" -> TransactionAppliedToLocalUTxO <$> fromCBOR <*> fromCBOR
      "SnapshotRequestDecided" -> SnapshotRequestDecided <$> fromCBOR
      "SnapshotRequested" -> SnapshotRequested <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "PartySignedSnapshot" -> PartySignedSnapshot <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "SnapshotConfirmed" -> SnapshotConfirmed <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "DepositRecorded" -> DepositRecorded <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "DepositActivated" -> DepositActivated <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "DepositExpired" -> DepositExpired <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "DepositRecovered" -> DepositRecovered <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "CommitApproved" -> CommitApproved <$> fromCBOR <*> fromCBOR
      "CommitFinalized" -> CommitFinalized <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "DecommitRecorded" -> DecommitRecorded <$> fromCBOR <*> fromCBOR
      "DecommitApproved" -> DecommitApproved <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "DecommitInvalid" -> DecommitInvalid <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "DecommitFinalized" -> DecommitFinalized <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "HeadClosed" -> HeadClosed <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "HeadContested" -> HeadContested <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "HeadIsReadyToFanout" -> HeadIsReadyToFanout <$> fromCBOR
      "HeadFanoutInitiated" -> HeadFanoutInitiated <$> fromCBOR <*> fromCBOR
      "HeadPartialFanoutSelected" -> HeadPartialFanoutSelected <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "HeadFanoutReverted" -> HeadFanoutReverted <$> fromCBOR
      "HeadFannedOut" -> HeadFannedOut <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "HeadPartialFannedOut" -> HeadPartialFannedOut <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "ChainRolledBack" -> ChainRolledBack <$> fromCBOR
      "TickObserved" -> TickObserved <$> fromCBOR <*> fromCBOR
      "IgnoredHeadInitializing" -> IgnoredHeadInitializing <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "TxInvalid" -> TxInvalid <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "LocalStateCleared" -> LocalStateCleared <$> fromCBOR <*> fromCBOR
      "Checkpoint" -> Checkpoint <$> fromCBOR
      "NodeUnsynced" -> NodeUnsynced <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "NodeSynced" -> NodeSynced <$> fromCBOR <*> fromCBOR <*> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded StateChanged"

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
