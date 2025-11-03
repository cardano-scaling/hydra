{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ServerOutput where

import Control.Lens ((.~))
import Data.Aeson (Value (..), defaultOptions, encode, genericParseJSON, genericToJSON, omitNothingFields, tagSingleConstructors, withObject, (.:))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (atKey, key)
import Data.ByteString.Lazy qualified as LBS
import Hydra.API.ClientInput (ClientInput)
import Hydra.Chain (PostChainTx, PostTxError)
import Hydra.Chain.ChainState (ChainSlot, IsChainState)
import Hydra.HeadLogic.Error (SideLoadRequirementFailure)
import Hydra.HeadLogic.State (ClosedState (..), HeadState (..), InitialState (..), OpenState (..), SeenSnapshot (..))
import Hydra.HeadLogic.State qualified as HeadState
import Hydra.Ledger (ValidationError)
import Hydra.Network (Host, ProtocolVersion)
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (NodeState, SyncedStatus)
import Hydra.Prelude hiding (seq)
import Hydra.Tx (HeadId, Party, Snapshot, SnapshotNumber, getSnapshot)
import Hydra.Tx qualified as Tx
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Crypto (MultiSignature)
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import Hydra.Tx.Snapshot qualified as HeadState

-- | The type of messages sent to clients by the 'Hydra.API.Server'.
data TimedServerOutput tx = TimedServerOutput
  { output :: ServerOutput tx
  , seq :: Natural
  , time :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance IsChainState tx => ToJSON (TimedServerOutput tx) where
  toJSON TimedServerOutput{output, seq, time} =
    case toJSON output of
      Object o ->
        Object $ o <> KeyMap.fromList [("seq", toJSON seq), ("timestamp", toJSON time)]
      _NotAnObject -> error "expected ServerOutput to serialize to an Object"

instance IsChainState tx => FromJSON (TimedServerOutput tx) where
  parseJSON v = flip (withObject "TimedServerOutput") v $ \o ->
    TimedServerOutput <$> parseJSON v <*> o .: "seq" <*> o .: "timestamp"

data DecommitInvalidReason tx
  = DecommitTxInvalid {localUTxO :: UTxOType tx, validationError :: ValidationError}
  | DecommitAlreadyInFlight {otherDecommitTxId :: TxIdType tx}
  deriving stock (Generic)

deriving stock instance (Eq (TxIdType tx), Eq (UTxOType tx)) => Eq (DecommitInvalidReason tx)
deriving stock instance (Show (TxIdType tx), Show (UTxOType tx)) => Show (DecommitInvalidReason tx)

instance (ToJSON (TxIdType tx), ToJSON (UTxOType tx)) => ToJSON (DecommitInvalidReason tx) where
  toJSON = genericToJSON defaultOptions

instance (FromJSON (TxIdType tx), FromJSON (UTxOType tx)) => FromJSON (DecommitInvalidReason tx) where
  parseJSON = genericParseJSON defaultOptions

-- | Individual messages as produced by the 'Hydra.HeadLogic' in
-- the 'ClientEffect'.
data ClientMessage tx
  = CommandFailed {clientInput :: ClientInput tx, state :: HeadState tx}
  | PostTxOnChainFailed {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
  | RejectedInputBecauseUnsynced {clientInput :: ClientInput tx, drift :: NominalDiffTime}
  | SideLoadSnapshotRejected {clientInput :: ClientInput tx, requirementFailure :: SideLoadRequirementFailure tx}
  | SyncedStatusReport {chainSlot :: ChainSlot, chainTime :: UTCTime, drift :: NominalDiffTime, synced :: SyncedStatus}
  deriving (Eq, Show, Generic)

instance IsChainState tx => ToJSON (ClientMessage tx) where
  toJSON =
    genericToJSON
      defaultOptions
        { omitNothingFields = True
        }

instance IsChainState tx => FromJSON (ClientMessage tx) where
  parseJSON =
    genericParseJSON
      defaultOptions
        { omitNothingFields = True
        }

-- | A friendly welcome message which tells a client something about the
-- node. Currently used for knowing what signing key the server uses (it
-- only knows one), 'HeadStatus' and optionally (if 'HeadIsOpen' or
-- 'SnapshotConfirmed' message is emitted) UTxO's present in the Hydra Head.
data Greetings tx = Greetings
  { me :: Party
  , headStatus :: HeadStatus
  , hydraHeadId :: Maybe HeadId
  , snapshotUtxo :: Maybe (UTxOType tx)
  , hydraNodeVersion :: String
  , env :: Environment
  , networkInfo :: NetworkInfo
  , chainSyncedStatus :: SyncedStatus
  , currentSlot :: ChainSlot
  }
  deriving (Generic)

deriving instance IsChainState tx => Eq (Greetings tx)
deriving instance IsChainState tx => Show (Greetings tx)

instance IsChainState tx => ToJSON (Greetings tx) where
  toJSON =
    genericToJSON
      defaultOptions
        { omitNothingFields = True
        , tagSingleConstructors = True
        }

instance IsChainState tx => FromJSON (Greetings tx) where
  parseJSON =
    genericParseJSON
      defaultOptions
        { omitNothingFields = True
        , tagSingleConstructors = True
        }

data InvalidInput = InvalidInput
  { reason :: String
  , input :: Text
  }
  deriving (Eq, Show, Generic)

deriving instance ToJSON InvalidInput
deriving instance FromJSON InvalidInput

data ServerOutput tx
  = NetworkConnected
  | NetworkDisconnected
  | NetworkVersionMismatch
      { ourVersion :: ProtocolVersion
      , theirVersion :: Maybe ProtocolVersion
      }
  | NetworkClusterIDMismatch
      { clusterPeers :: Text
      , misconfiguredPeers :: Text
      }
  | PeerConnected {peer :: Host}
  | PeerDisconnected {peer :: Host}
  | HeadIsInitializing {headId :: HeadId, parties :: [Party]}
  | Committed {headId :: HeadId, party :: Party, utxo :: UTxOType tx}
  | HeadIsOpen {headId :: HeadId, utxo :: UTxOType tx}
  | HeadIsClosed
      { headId :: HeadId
      , snapshotNumber :: SnapshotNumber
      , contestationDeadline :: UTCTime
      -- ^ Nominal deadline until which contest can be submitted and after
      -- which fanout is possible. NOTE: Use this only for informational
      -- purpose and wait for 'ReadyToFanout' instead before sending 'Fanout'
      -- as the ledger of our cardano-node might not have progressed
      -- sufficiently in time yet and we do not re-submit transactions (yet).
      }
  | HeadIsContested {headId :: HeadId, snapshotNumber :: SnapshotNumber, contestationDeadline :: UTCTime}
  | ReadyToFanout {headId :: HeadId}
  | HeadIsAborted {headId :: HeadId, utxo :: UTxOType tx}
  | HeadIsFinalized {headId :: HeadId, utxo :: UTxOType tx}
  | -- | Given transaction has been seen as valid in the Head. It is expected to
    -- eventually be part of a 'SnapshotConfirmed'.
    TxValid {headId :: HeadId, transactionId :: TxIdType tx}
  | -- | Given transaction was not not applicable to the given UTxO in time and
    -- has been dropped.
    TxInvalid {headId :: HeadId, utxo :: UTxOType tx, transaction :: tx, validationError :: ValidationError}
  | -- | Given snapshot was confirmed and included transactions can be
    -- considered final.
    SnapshotConfirmed
      { headId :: HeadId
      , snapshot :: Snapshot tx
      , signatures :: MultiSignature (Snapshot tx)
      }
  | IgnoredHeadInitializing
      { headId :: HeadId
      , contestationPeriod :: ContestationPeriod
      , parties :: [Party]
      , participants :: [OnChainId]
      }
  | DecommitRequested {headId :: HeadId, decommitTx :: tx, utxoToDecommit :: UTxOType tx}
  | DecommitInvalid {headId :: HeadId, decommitTx :: tx, decommitInvalidReason :: DecommitInvalidReason tx}
  | DecommitApproved {headId :: HeadId, decommitTxId :: TxIdType tx, utxoToDecommit :: UTxOType tx}
  | DecommitFinalized {headId :: HeadId, distributedUTxO :: UTxOType tx}
  | -- XXX: Rename to DepositRecorded following the state events naming. But only
    -- do this when changing the endpoint also to /commits
    CommitRecorded
      { headId :: HeadId
      , utxoToCommit :: UTxOType tx
      , -- XXX: Inconsinstent field name
        pendingDeposit :: TxIdType tx
      , deadline :: UTCTime
      }
  | DepositActivated {headId :: HeadId, depositTxId :: TxIdType tx, deadline :: UTCTime, chainTime :: UTCTime}
  | DepositExpired {headId :: HeadId, depositTxId :: TxIdType tx, deadline :: UTCTime, chainTime :: UTCTime}
  | CommitApproved {headId :: HeadId, utxoToCommit :: UTxOType tx}
  | CommitFinalized {headId :: HeadId, depositTxId :: TxIdType tx}
  | -- XXX: Rename to DepositRecovered to be more consistent. But only do this
    -- when changing the endpoint also to /commits
    CommitRecovered {headId :: HeadId, recoveredUTxO :: UTxOType tx, recoveredTxId :: TxIdType tx}
  | -- | Snapshot was side-loaded, and the included transactions can be considered final.
    -- The local state has been reset, meaning pending transactions were pruned.
    -- Any signing round has been discarded, and the snapshot leader has changed accordingly.
    SnapshotSideLoaded {headId :: HeadId, snapshotNumber :: SnapshotNumber}
  | EventLogRotated {checkpoint :: NodeState tx}
  | NodeUnsynced {chainSlot :: ChainSlot, chainTime :: UTCTime, drift :: NominalDiffTime}
  | NodeSynced {chainSlot :: ChainSlot, chainTime :: UTCTime, drift :: NominalDiffTime}
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (ServerOutput tx)
deriving stock instance IsChainState tx => Show (ServerOutput tx)
deriving anyclass instance IsChainState tx => FromJSON (ServerOutput tx)
deriving anyclass instance IsChainState tx => ToJSON (ServerOutput tx)

-- | Whether or not to include full UTxO in server outputs.
data WithUTxO = WithUTxO | WithoutUTxO
  deriving stock (Eq, Show)

-- | Whether or not to filter transaction server outputs by given address.
data WithAddressedTx = WithAddressedTx Text | WithoutAddressedTx
  deriving stock (Eq, Show)

data ServerOutputConfig = ServerOutputConfig
  { utxoInSnapshot :: WithUTxO
  , addressInTx :: WithAddressedTx
  }
  deriving stock (Eq, Show)

-- | Replaces the json encoded tx field with it's cbor representation.
--
-- NOTE: we deliberately pattern match on all 'ServerOutput' constructors in
-- 'handleTxOutput' so that we don't forget to update this function if they
-- change.
prepareServerOutput ::
  IsChainState tx =>
  -- | Decide on tx representation
  ServerOutputConfig ->
  -- | Server output
  TimedServerOutput tx ->
  -- | Final output
  LBS.ByteString
prepareServerOutput config response =
  case output response of
    Committed{} -> encodedResponse
    HeadIsInitializing{} -> encodedResponse
    HeadIsOpen{} -> encodedResponse
    HeadIsClosed{} -> encodedResponse
    HeadIsContested{} -> encodedResponse
    ReadyToFanout{} -> encodedResponse
    HeadIsAborted{} -> encodedResponse
    HeadIsFinalized{} -> encodedResponse
    TxValid{} -> encodedResponse
    TxInvalid{} -> encodedResponse
    SnapshotConfirmed{} ->
      handleUtxoInclusion config removeSnapshotUTxO encodedResponse
    IgnoredHeadInitializing{} -> encodedResponse
    DecommitRequested{} -> encodedResponse
    DecommitApproved{} -> encodedResponse
    DecommitFinalized{} -> encodedResponse
    DecommitInvalid{} -> encodedResponse
    CommitRecorded{} -> encodedResponse
    DepositActivated{} -> encodedResponse
    DepositExpired{} -> encodedResponse
    CommitApproved{} -> encodedResponse
    CommitFinalized{} -> encodedResponse
    CommitRecovered{} -> encodedResponse
    NetworkConnected -> encodedResponse
    NetworkDisconnected -> encodedResponse
    NetworkVersionMismatch{} -> encodedResponse
    NetworkClusterIDMismatch{} -> encodedResponse
    PeerConnected{} -> encodedResponse
    PeerDisconnected{} -> encodedResponse
    SnapshotSideLoaded{} -> encodedResponse
    EventLogRotated{} -> encodedResponse
    NodeUnsynced{} -> encodedResponse
    NodeSynced{} -> encodedResponse
 where
  encodedResponse = encode response

removeSnapshotUTxO :: LBS.ByteString -> LBS.ByteString
removeSnapshotUTxO = key "snapshot" . atKey "utxo" .~ Nothing

handleUtxoInclusion :: ServerOutputConfig -> (a -> a) -> a -> a
handleUtxoInclusion config f bs =
  case utxoInSnapshot config of
    WithUTxO -> bs
    WithoutUTxO -> bs & f

-- | All possible Hydra states displayed in the API server outputs.
data HeadStatus
  = Idle
  | Initializing
  | Open
  | Closed
  | FanoutPossible
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | All information needed to distinguish behavior of the commit endpoint.
data CommitInfo
  = CannotCommit
  | NormalCommit HeadId
  | IncrementalCommit HeadId

-- | L2 Hydra network status information.
data NetworkInfo = NetworkInfo
  { networkConnected :: Bool
  , peersInfo :: Map Host Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Get latest confirmed snapshot UTxO from 'HeadState'.
getSnapshotUtxo :: IsTx tx => HeadState tx -> Maybe (UTxOType tx)
getSnapshotUtxo = \case
  HeadState.Idle{} ->
    Nothing
  HeadState.Initial InitialState{committed} ->
    let u0 = fold committed
     in Just u0
  HeadState.Open OpenState{coordinatedHeadState} ->
    let snapshot = getSnapshot coordinatedHeadState.confirmedSnapshot
     in Just $ Tx.utxo snapshot <> fromMaybe mempty (Tx.utxoToCommit snapshot)
  HeadState.Closed ClosedState{confirmedSnapshot} ->
    let snapshot = getSnapshot confirmedSnapshot
     in Just $ Tx.utxo snapshot <> fromMaybe mempty (Tx.utxoToCommit snapshot)

-- | Get latest seen snapshot from 'HeadState'.
getSeenSnapshot :: IsTx tx => HeadState tx -> HeadState.SeenSnapshot tx
getSeenSnapshot = \case
  HeadState.Idle{} ->
    NoSeenSnapshot
  HeadState.Initial{} ->
    NoSeenSnapshot
  HeadState.Open OpenState{coordinatedHeadState} ->
    coordinatedHeadState.seenSnapshot
  HeadState.Closed ClosedState{confirmedSnapshot} ->
    let Snapshot{number} = getSnapshot confirmedSnapshot
     in LastSeenSnapshot number

-- | Get latest confirmed snapshot from 'HeadState'.
getConfirmedSnapshot :: IsChainState tx => HeadState tx -> Maybe (HeadState.ConfirmedSnapshot tx)
getConfirmedSnapshot = \case
  HeadState.Idle{} ->
    Nothing
  HeadState.Initial InitialState{headId, committed} ->
    let u0 = fold committed
     in Just $ InitialSnapshot headId u0
  HeadState.Open OpenState{coordinatedHeadState} ->
    Just coordinatedHeadState.confirmedSnapshot
  HeadState.Closed ClosedState{confirmedSnapshot} ->
    Just confirmedSnapshot
