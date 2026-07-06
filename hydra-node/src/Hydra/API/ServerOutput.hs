{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ServerOutput (
  module Hydra.API.ServerOutput,
  ApiEncoding (..),
) where

import Cardano.Binary (Decoder)
import Control.Lens ((.~))
import Data.Aeson (Value (..), defaultOptions, encode, genericParseJSON, genericToJSON, omitNothingFields, tagSingleConstructors, withObject, (.:))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (atKey, key)
import Data.ByteString.Lazy qualified as LBS
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.WireFormat (ApiEncoding (..))
import Hydra.Chain (PostChainTx, PostTxError)
import Hydra.Chain.ChainState (ChainSlot, IsChainState)
import Hydra.HeadLogic.Error (SideLoadRequirementFailure)
import Hydra.HeadLogic.State (ClosedState (..), HeadState, OpenState (..), SeenSnapshot (..))
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
import Hydra.Tx.Snapshot (Snapshot (..))
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

-- NOTE: Unlike the JSON instance, which merges 'seq' and 'timestamp' into the
-- inner 'ServerOutput' object, the CBOR encoding is a plain tagged envelope.
-- The tag makes any server-sent message start with a unique text token, so
-- clients can dispatch on it (see 'ApiMessage').
instance IsChainState tx => ToCBOR (TimedServerOutput tx) where
  toCBOR TimedServerOutput{output, seq, time} =
    toCBOR ("TimedServerOutput" :: Text) <> toCBOR seq <> toCBOR time <> toCBOR output

instance IsChainState tx => FromCBOR (TimedServerOutput tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("TimedServerOutput" :: Text) -> decodeTimedServerOutputBody
      tag -> fail $ show tag <> " is not a proper CBOR-encoded TimedServerOutput"

-- | Decode a 'TimedServerOutput' after its @TimedServerOutput@ tag has already
-- been consumed (used for tag-based dispatch in 'ApiMessage').
decodeTimedServerOutputBody :: IsChainState tx => Decoder s (TimedServerOutput tx)
decodeTimedServerOutputBody = do
  seq <- fromCBOR
  time <- fromCBOR
  output <- fromCBOR
  pure TimedServerOutput{output, seq, time}

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

instance IsTx tx => ToCBOR (DecommitInvalidReason tx) where
  toCBOR = \case
    DecommitTxInvalid{localUTxO, validationError} ->
      toCBOR ("DecommitTxInvalid" :: Text) <> toCBOR localUTxO <> toCBOR validationError
    DecommitAlreadyInFlight{otherDecommitTxId} ->
      toCBOR ("DecommitAlreadyInFlight" :: Text) <> toCBOR otherDecommitTxId

instance IsTx tx => FromCBOR (DecommitInvalidReason tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("DecommitTxInvalid" :: Text) -> DecommitTxInvalid <$> fromCBOR <*> fromCBOR
      "DecommitAlreadyInFlight" -> DecommitAlreadyInFlight <$> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded DecommitInvalidReason"

-- | Individual messages as produced by the 'Hydra.HeadLogic' in
-- the 'ClientEffect'.
data ClientMessage tx
  = CommandFailed {clientInput :: ClientInput tx, state :: HeadState tx}
  | PostTxOnChainFailed {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
  | RejectedInputBecauseUnsynced {clientInput :: ClientInput tx, drift :: NominalDiffTime}
  | SideLoadSnapshotRejected {clientInput :: ClientInput tx, requirementFailure :: SideLoadRequirementFailure tx}
  | SyncedStatusReport {chainSlot :: ChainSlot, chainTime :: UTCTime, drift :: NominalDiffTime, synced :: SyncedStatus}
  deriving stock (Eq, Show, Generic)

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

instance IsChainState tx => ToCBOR (ClientMessage tx) where
  toCBOR = \case
    CommandFailed{clientInput, state = headState} ->
      toCBOR ("CommandFailed" :: Text) <> toCBOR clientInput <> toCBOR headState
    PostTxOnChainFailed{postChainTx, postTxError} ->
      toCBOR ("PostTxOnChainFailed" :: Text) <> toCBOR postChainTx <> toCBOR postTxError
    RejectedInputBecauseUnsynced{clientInput, drift} ->
      toCBOR ("RejectedInputBecauseUnsynced" :: Text) <> toCBOR clientInput <> toCBOR drift
    SideLoadSnapshotRejected{clientInput, requirementFailure} ->
      toCBOR ("SideLoadSnapshotRejected" :: Text) <> toCBOR clientInput <> toCBOR requirementFailure
    SyncedStatusReport{chainSlot, chainTime, drift, synced} ->
      toCBOR ("SyncedStatusReport" :: Text)
        <> toCBOR chainSlot
        <> toCBOR chainTime
        <> toCBOR drift
        <> toCBOR synced

instance IsChainState tx => FromCBOR (ClientMessage tx) where
  fromCBOR = fromCBOR >>= decodeClientMessageBody

-- | Decode a 'ClientMessage' given its already-decoded constructor tag (used
-- for tag-based dispatch in 'ApiMessage').
decodeClientMessageBody :: IsChainState tx => Text -> Decoder s (ClientMessage tx)
decodeClientMessageBody = \case
  "CommandFailed" -> CommandFailed <$> fromCBOR <*> fromCBOR
  "PostTxOnChainFailed" -> PostTxOnChainFailed <$> fromCBOR <*> fromCBOR
  "RejectedInputBecauseUnsynced" -> RejectedInputBecauseUnsynced <$> fromCBOR <*> fromCBOR
  "SideLoadSnapshotRejected" -> SideLoadSnapshotRejected <$> fromCBOR <*> fromCBOR
  "SyncedStatusReport" -> SyncedStatusReport <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
  tag -> fail $ show tag <> " is not a proper CBOR-encoded ClientMessage"

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
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (Greetings tx)
deriving stock instance IsChainState tx => Show (Greetings tx)

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

instance IsChainState tx => ToCBOR (Greetings tx) where
  toCBOR Greetings{me, headStatus, hydraHeadId, snapshotUtxo, hydraNodeVersion, env, networkInfo, chainSyncedStatus, currentSlot} =
    toCBOR ("Greetings" :: Text)
      <> toCBOR me
      <> toCBOR headStatus
      <> toCBOR hydraHeadId
      <> toCBOR snapshotUtxo
      <> toCBOR (toText hydraNodeVersion)
      <> toCBOR env
      <> toCBOR networkInfo
      <> toCBOR chainSyncedStatus
      <> toCBOR currentSlot

instance IsChainState tx => FromCBOR (Greetings tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("Greetings" :: Text) -> decodeGreetingsBody
      tag -> fail $ show tag <> " is not a proper CBOR-encoded Greetings"

-- | Decode a 'Greetings' after its @Greetings@ tag has already been consumed
-- (used for tag-based dispatch in 'ApiMessage').
decodeGreetingsBody :: IsChainState tx => Decoder s (Greetings tx)
decodeGreetingsBody = do
  me <- fromCBOR
  headStatus <- fromCBOR
  hydraHeadId <- fromCBOR
  snapshotUtxo <- fromCBOR
  hydraNodeVersion <- toString <$> fromCBOR @Text
  env <- fromCBOR
  networkInfo <- fromCBOR
  chainSyncedStatus <- fromCBOR
  currentSlot <- fromCBOR
  pure Greetings{me, headStatus, hydraHeadId, snapshotUtxo, hydraNodeVersion, env, networkInfo, chainSyncedStatus, currentSlot}

data InvalidInput = InvalidInput
  { reason :: String
  , input :: Text
  }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance ToJSON InvalidInput
deriving anyclass instance FromJSON InvalidInput

instance ToCBOR InvalidInput where
  toCBOR InvalidInput{reason, input} =
    toCBOR ("InvalidInput" :: Text) <> toCBOR (toText reason) <> toCBOR input

instance FromCBOR InvalidInput where
  fromCBOR =
    fromCBOR >>= \case
      ("InvalidInput" :: Text) -> decodeInvalidInputBody
      tag -> fail $ show tag <> " is not a proper CBOR-encoded InvalidInput"

-- | Decode an 'InvalidInput' after its @InvalidInput@ tag has already been
-- consumed (used for tag-based dispatch in 'ApiMessage').
decodeInvalidInputBody :: Decoder s InvalidInput
decodeInvalidInputBody = do
  reason <- toString <$> fromCBOR @Text
  input <- fromCBOR
  pure InvalidInput{reason, input}

-- | Union of all messages the hydra-node sends to clients. Only used for
-- decoding on the client side; the server encodes and sends the individual
-- types directly (their encodings are the same as the union's).
--
-- In CBOR, every server-sent message starts with a text tag that is unique
-- across the whole API surface, so a single tag read suffices to dispatch.
data ApiMessage tx
  = ApiTimedServerOutput (TimedServerOutput tx)
  | ApiClientMessage (ClientMessage tx)
  | ApiGreetings (Greetings tx)
  | ApiInvalidInput InvalidInput
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (ApiMessage tx)
deriving stock instance IsChainState tx => Show (ApiMessage tx)

instance IsChainState tx => ToJSON (ApiMessage tx) where
  toJSON = \case
    ApiTimedServerOutput o -> toJSON o
    ApiClientMessage m -> toJSON m
    ApiGreetings g -> toJSON g
    ApiInvalidInput i -> toJSON i

instance IsChainState tx => FromJSON (ApiMessage tx) where
  parseJSON v =
    (ApiTimedServerOutput <$> parseJSON v)
      <|> (ApiClientMessage <$> parseJSON v)
      <|> (ApiGreetings <$> parseJSON v)
      <|> (ApiInvalidInput <$> parseJSON v)

instance IsChainState tx => ToCBOR (ApiMessage tx) where
  toCBOR = \case
    ApiTimedServerOutput o -> toCBOR o
    ApiClientMessage m -> toCBOR m
    ApiGreetings g -> toCBOR g
    ApiInvalidInput i -> toCBOR i

instance IsChainState tx => FromCBOR (ApiMessage tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("TimedServerOutput" :: Text) -> ApiTimedServerOutput <$> decodeTimedServerOutputBody
      "Greetings" -> ApiGreetings <$> decodeGreetingsBody
      "InvalidInput" -> ApiInvalidInput <$> decodeInvalidInputBody
      tag -> ApiClientMessage <$> decodeClientMessageBody tag

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
  | HeadIsOpen {headId :: HeadId, parties :: [Party]}
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
  | HeadIsFinalized {headId :: HeadId, finalizedUTxO :: UTxOType tx}
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
  | -- TODO: Rename to DepositRecorded following the state events naming. But only
    -- do this when changing the endpoint also to /deposits
    CommitRecorded
      { headId :: HeadId
      , utxoToCommit :: UTxOType tx
      , -- XXX: Inconsinstent field name
        pendingDeposit :: TxIdType tx
      , deadline :: UTCTime
      }
  | DepositActivated {headId :: HeadId, depositTxId :: TxIdType tx, deadline :: UTCTime, chainTime :: UTCTime}
  | DepositExpired {headId :: HeadId, depositTxId :: TxIdType tx, deadline :: UTCTime, chainTime :: UTCTime}
  | -- TODO: Rename to DepositApproved
    CommitApproved {headId :: HeadId, utxoToCommit :: UTxOType tx}
  | -- TODO: Rename to DepositFinalized
    CommitFinalized {headId :: HeadId, depositTxId :: TxIdType tx}
  | -- TODO: Rename to DepositRecovered to be more consistent. But only do this
    -- when changing the endpoint also to /deposits
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

instance IsChainState tx => ToCBOR (ServerOutput tx) where
  toCBOR = \case
    NetworkConnected ->
      toCBOR ("NetworkConnected" :: Text)
    NetworkDisconnected ->
      toCBOR ("NetworkDisconnected" :: Text)
    NetworkVersionMismatch{ourVersion, theirVersion} ->
      toCBOR ("NetworkVersionMismatch" :: Text) <> toCBOR ourVersion <> toCBOR theirVersion
    NetworkClusterIDMismatch{clusterPeers, misconfiguredPeers} ->
      toCBOR ("NetworkClusterIDMismatch" :: Text) <> toCBOR clusterPeers <> toCBOR misconfiguredPeers
    PeerConnected{peer} ->
      toCBOR ("PeerConnected" :: Text) <> toCBOR peer
    PeerDisconnected{peer} ->
      toCBOR ("PeerDisconnected" :: Text) <> toCBOR peer
    HeadIsOpen{headId, parties} ->
      toCBOR ("HeadIsOpen" :: Text) <> toCBOR headId <> toCBOR parties
    HeadIsClosed{headId, snapshotNumber, contestationDeadline} ->
      toCBOR ("HeadIsClosed" :: Text)
        <> toCBOR headId
        <> toCBOR snapshotNumber
        <> toCBOR contestationDeadline
    HeadIsContested{headId, snapshotNumber, contestationDeadline} ->
      toCBOR ("HeadIsContested" :: Text)
        <> toCBOR headId
        <> toCBOR snapshotNumber
        <> toCBOR contestationDeadline
    ReadyToFanout{headId} ->
      toCBOR ("ReadyToFanout" :: Text) <> toCBOR headId
    HeadIsFinalized{headId, finalizedUTxO} ->
      toCBOR ("HeadIsFinalized" :: Text) <> toCBOR headId <> toCBOR finalizedUTxO
    TxValid{headId, transactionId} ->
      toCBOR ("TxValid" :: Text) <> toCBOR headId <> toCBOR transactionId
    TxInvalid{headId, utxo, transaction, validationError} ->
      toCBOR ("TxInvalid" :: Text)
        <> toCBOR headId
        <> toCBOR utxo
        <> toCBOR transaction
        <> toCBOR validationError
    SnapshotConfirmed{headId, snapshot, signatures} ->
      toCBOR ("SnapshotConfirmed" :: Text)
        <> toCBOR headId
        <> toCBOR snapshot
        <> toCBOR signatures
    IgnoredHeadInitializing{headId, contestationPeriod, parties, participants} ->
      toCBOR ("IgnoredHeadInitializing" :: Text)
        <> toCBOR headId
        <> toCBOR contestationPeriod
        <> toCBOR parties
        <> toCBOR participants
    DecommitRequested{headId, decommitTx, utxoToDecommit} ->
      toCBOR ("DecommitRequested" :: Text)
        <> toCBOR headId
        <> toCBOR decommitTx
        <> toCBOR utxoToDecommit
    DecommitInvalid{headId, decommitTx, decommitInvalidReason} ->
      toCBOR ("DecommitInvalid" :: Text)
        <> toCBOR headId
        <> toCBOR decommitTx
        <> toCBOR decommitInvalidReason
    DecommitApproved{headId, decommitTxId, utxoToDecommit} ->
      toCBOR ("DecommitApproved" :: Text)
        <> toCBOR headId
        <> toCBOR decommitTxId
        <> toCBOR utxoToDecommit
    DecommitFinalized{headId, distributedUTxO} ->
      toCBOR ("DecommitFinalized" :: Text) <> toCBOR headId <> toCBOR distributedUTxO
    CommitRecorded{headId, utxoToCommit, pendingDeposit, deadline} ->
      toCBOR ("CommitRecorded" :: Text)
        <> toCBOR headId
        <> toCBOR utxoToCommit
        <> toCBOR pendingDeposit
        <> toCBOR deadline
    DepositActivated{headId, depositTxId, deadline, chainTime} ->
      toCBOR ("DepositActivated" :: Text)
        <> toCBOR headId
        <> toCBOR depositTxId
        <> toCBOR deadline
        <> toCBOR chainTime
    DepositExpired{headId, depositTxId, deadline, chainTime} ->
      toCBOR ("DepositExpired" :: Text)
        <> toCBOR headId
        <> toCBOR depositTxId
        <> toCBOR deadline
        <> toCBOR chainTime
    CommitApproved{headId, utxoToCommit} ->
      toCBOR ("CommitApproved" :: Text) <> toCBOR headId <> toCBOR utxoToCommit
    CommitFinalized{headId, depositTxId} ->
      toCBOR ("CommitFinalized" :: Text) <> toCBOR headId <> toCBOR depositTxId
    CommitRecovered{headId, recoveredUTxO, recoveredTxId} ->
      toCBOR ("CommitRecovered" :: Text)
        <> toCBOR headId
        <> toCBOR recoveredUTxO
        <> toCBOR recoveredTxId
    SnapshotSideLoaded{headId, snapshotNumber} ->
      toCBOR ("SnapshotSideLoaded" :: Text) <> toCBOR headId <> toCBOR snapshotNumber
    EventLogRotated{checkpoint} ->
      toCBOR ("EventLogRotated" :: Text) <> toCBOR checkpoint
    NodeUnsynced{chainSlot, chainTime, drift} ->
      toCBOR ("NodeUnsynced" :: Text) <> toCBOR chainSlot <> toCBOR chainTime <> toCBOR drift
    NodeSynced{chainSlot, chainTime, drift} ->
      toCBOR ("NodeSynced" :: Text) <> toCBOR chainSlot <> toCBOR chainTime <> toCBOR drift

instance IsChainState tx => FromCBOR (ServerOutput tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("NetworkConnected" :: Text) -> pure NetworkConnected
      "NetworkDisconnected" -> pure NetworkDisconnected
      "NetworkVersionMismatch" -> NetworkVersionMismatch <$> fromCBOR <*> fromCBOR
      "NetworkClusterIDMismatch" -> NetworkClusterIDMismatch <$> fromCBOR <*> fromCBOR
      "PeerConnected" -> PeerConnected <$> fromCBOR
      "PeerDisconnected" -> PeerDisconnected <$> fromCBOR
      "HeadIsOpen" -> HeadIsOpen <$> fromCBOR <*> fromCBOR
      "HeadIsClosed" -> HeadIsClosed <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "HeadIsContested" -> HeadIsContested <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "ReadyToFanout" -> ReadyToFanout <$> fromCBOR
      "HeadIsFinalized" -> HeadIsFinalized <$> fromCBOR <*> fromCBOR
      "TxValid" -> TxValid <$> fromCBOR <*> fromCBOR
      "TxInvalid" -> TxInvalid <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "SnapshotConfirmed" -> SnapshotConfirmed <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "IgnoredHeadInitializing" -> IgnoredHeadInitializing <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "DecommitRequested" -> DecommitRequested <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "DecommitInvalid" -> DecommitInvalid <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "DecommitApproved" -> DecommitApproved <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "DecommitFinalized" -> DecommitFinalized <$> fromCBOR <*> fromCBOR
      "CommitRecorded" -> CommitRecorded <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "DepositActivated" -> DepositActivated <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "DepositExpired" -> DepositExpired <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "CommitApproved" -> CommitApproved <$> fromCBOR <*> fromCBOR
      "CommitFinalized" -> CommitFinalized <$> fromCBOR <*> fromCBOR
      "CommitRecovered" -> CommitRecovered <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "SnapshotSideLoaded" -> SnapshotSideLoaded <$> fromCBOR <*> fromCBOR
      "EventLogRotated" -> EventLogRotated <$> fromCBOR
      "NodeUnsynced" -> NodeUnsynced <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "NodeSynced" -> NodeSynced <$> fromCBOR <*> fromCBOR <*> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded ServerOutput"

-- | Whether or not to include full UTxO in server outputs.
data WithUTxO = WithUTxO | WithoutUTxO
  deriving stock (Eq, Show)

-- | Whether or not to filter transaction server outputs by given address.
data WithAddressedTx = WithAddressedTx Text | WithoutAddressedTx
  deriving stock (Eq, Show)

data ServerOutputConfig = ServerOutputConfig
  { utxoInSnapshot :: WithUTxO
  , addressInTx :: WithAddressedTx
  , encoding :: ApiEncoding
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
    HeadIsOpen{} -> encodedResponse
    HeadIsClosed{} -> encodedResponse
    HeadIsContested{} -> encodedResponse
    ReadyToFanout{} -> encodedResponse
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

-- | Typed variant of 'removeSnapshotUTxO', used on CBOR connections where the
-- byte-level JSON surgery does not apply: with 'WithoutUTxO', the snapshot's
-- utxo is replaced by 'mempty' before encoding.
--
-- NOTE: Unlike the JSON variant which drops the @utxo@ key entirely, this
-- yields an empty utxo set — both are display-only filters and not meant to
-- round-trip back into a valid 'Snapshot'.
handleUtxoInclusionTyped :: IsTx tx => ServerOutputConfig -> TimedServerOutput tx -> TimedServerOutput tx
handleUtxoInclusionTyped config timed =
  case utxoInSnapshot config of
    WithUTxO -> timed
    WithoutUTxO ->
      case output timed of
        SnapshotConfirmed{headId, snapshot = Snapshot{headId = snapHeadId, version, number, confirmed, utxoToCommit, utxoToDecommit, accumulator}, signatures} ->
          timed
            { output =
                SnapshotConfirmed
                  { headId
                  , snapshot =
                      Snapshot
                        { headId = snapHeadId
                        , version
                        , number
                        , confirmed
                        , utxo = mempty
                        , utxoToCommit
                        , utxoToDecommit
                        , accumulator
                        }
                  , signatures
                  }
            }
        _ -> timed

-- | All possible Hydra states displayed in the API server outputs.
data HeadStatus
  = Idle
  | Open
  | Closed
  | FanoutPossible
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR HeadStatus where
  toCBOR = \case
    Idle -> toCBOR ("Idle" :: Text)
    Open -> toCBOR ("Open" :: Text)
    Closed -> toCBOR ("Closed" :: Text)
    FanoutPossible -> toCBOR ("FanoutPossible" :: Text)

instance FromCBOR HeadStatus where
  fromCBOR =
    fromCBOR >>= \case
      ("Idle" :: Text) -> pure Idle
      "Open" -> pure Open
      "Closed" -> pure Closed
      "FanoutPossible" -> pure FanoutPossible
      tag -> fail $ show tag <> " is not a proper CBOR-encoded HeadStatus"

-- | All information needed to distinguish behavior of the commit endpoint.
data CommitInfo
  = CannotCommit
  | IncrementalCommit HeadId

-- | L2 Hydra network status information.
data NetworkInfo = NetworkInfo
  { networkConnected :: Bool
  , peersInfo :: Map Host Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR NetworkInfo where
  toCBOR NetworkInfo{networkConnected, peersInfo} =
    toCBOR networkConnected <> toCBOR peersInfo

instance FromCBOR NetworkInfo where
  fromCBOR = NetworkInfo <$> fromCBOR <*> fromCBOR

-- | Get latest confirmed snapshot UTxO from 'HeadState'.
getSnapshotUtxo :: IsTx tx => HeadState tx -> Maybe (UTxOType tx)
getSnapshotUtxo = \case
  HeadState.Idle{} ->
    Nothing
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
  HeadState.Open OpenState{coordinatedHeadState} ->
    coordinatedHeadState.seenSnapshot
  HeadState.Closed ClosedState{confirmedSnapshot} ->
    let Snapshot{number} = getSnapshot confirmedSnapshot
     in LastSeenSnapshot number

-- | Get latest confirmed snapshot from 'HeadState'.
getConfirmedSnapshot :: HeadState tx -> Maybe (HeadState.ConfirmedSnapshot tx)
getConfirmedSnapshot = \case
  HeadState.Idle{} ->
    Nothing
  HeadState.Open OpenState{coordinatedHeadState} ->
    Just coordinatedHeadState.confirmedSnapshot
  HeadState.Closed ClosedState{confirmedSnapshot} ->
    Just confirmedSnapshot
