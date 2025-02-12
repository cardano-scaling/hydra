{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ServerOutput where

import Control.Lens ((.~))
import Data.Aeson (Value (..), defaultOptions, encode, genericParseJSON, genericToJSON, omitNothingFields, withObject, (.:))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (atKey, key)
import Data.ByteString.Lazy qualified as LBS
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.Chain (PostChainTx, PostTxError)
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Outcome qualified as StateChanged
import Hydra.HeadLogic.State (HeadState)
import Hydra.Ledger (ValidationError)
import Hydra.Network (Host, NodeId)
import Hydra.Prelude hiding (seq, state)
import Hydra.Tx (
  HeadId,
  Party,
  Snapshot,
  SnapshotNumber,
 )
import Hydra.Tx qualified as Tx
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Crypto (MultiSignature)
import Hydra.Tx.IsTx (ArbitraryIsTx, IsTx (..))
import Hydra.Tx.OnChainId (OnChainId)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

-- | The type of messages sent to clients by the 'Hydra.API.Server'.
data TimedServerOutput tx = TimedServerOutput
  { output :: ServerOutput tx
  , seq :: Natural
  , time :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance Arbitrary (ServerOutput tx) => Arbitrary (TimedServerOutput tx) where
  arbitrary = genericArbitrary

-- | Generate a random timed server output given a normal server output.
genTimedServerOutput :: ServerOutput tx -> Gen (TimedServerOutput tx)
genTimedServerOutput o =
  TimedServerOutput o <$> arbitrary <*> arbitrary

instance IsChainState tx => ToJSON (TimedServerOutput tx) where
  toJSON TimedServerOutput{output, seq, time} =
    case toJSON output of
      Object o ->
        Object $ o <> KeyMap.fromList [("seq", toJSON seq), ("timestamp", toJSON time)]
      _NotAnObject -> error "expected ServerOutput to serialize to an Object"

instance IsChainState tx => FromJSON (TimedServerOutput tx) where
  parseJSON v = flip (withObject "TimedServerOutput") v $ \o ->
    TimedServerOutput <$> parseJSON v <*> o .: "seq" <*> o .: "timestamp"

-- | Individual server output messages as produced by the 'Hydra.HeadLogic' in
-- the 'ClientEffect'.
data ServerOutput tx
  = PeerConnected {peer :: NodeId}
  | PeerDisconnected {peer :: NodeId}
  | PeerHandshakeFailure
      { remoteHost :: Host
      , ourVersion :: Natural
      , theirVersions :: [Natural]
      }
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
  | CommandFailed {clientInput :: ClientInput tx, state :: HeadState tx}
  | -- | Given transaction has been seen as valid in the Head. It is expected to
    -- eventually be part of a 'SnapshotConfirmed'.
    TxValid {headId :: HeadId, transactionId :: TxIdType tx, transaction :: tx}
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
  | GetUTxOResponse {headId :: HeadId, utxo :: UTxOType tx}
  | InvalidInput {reason :: String, input :: Text}
  | -- | A friendly welcome message which tells a client something about the
    -- node. Currently used for knowing what signing key the server uses (it
    -- only knows one), 'HeadStatus' and optionally (if 'HeadIsOpen' or
    -- 'SnapshotConfirmed' message is emitted) UTxO's present in the Hydra Head.
    Greetings
      { me :: Party
      , headStatus :: StateChanged.HeadStatus
      , hydraHeadId :: Maybe HeadId
      , snapshotUtxo :: Maybe (UTxOType tx)
      , hydraNodeVersion :: String
      }
  | PostTxOnChainFailed {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
  | IgnoredHeadInitializing
      { headId :: HeadId
      , contestationPeriod :: ContestationPeriod
      , parties :: [Party]
      , participants :: [OnChainId]
      }
  | DecommitRequested {headId :: HeadId, decommitTx :: tx, utxoToDecommit :: UTxOType tx}
  | DecommitInvalid {headId :: HeadId, decommitTx :: tx, decommitInvalidReason :: StateChanged.DecommitInvalidReason tx}
  | DecommitApproved {headId :: HeadId, decommitTxId :: TxIdType tx, utxoToDecommit :: UTxOType tx}
  | DecommitFinalized {headId :: HeadId, decommitTxId :: TxIdType tx}
  | CommitRecorded {headId :: HeadId, utxoToCommit :: UTxOType tx, pendingDeposit :: TxIdType tx, deadline :: UTCTime}
  | CommitApproved {headId :: HeadId, utxoToCommit :: UTxOType tx}
  | CommitFinalized {headId :: HeadId, theDeposit :: TxIdType tx}
  | CommitRecovered {headId :: HeadId, recoveredUTxO :: UTxOType tx, recoveredTxId :: TxIdType tx}
  | CommitIgnored {headId :: HeadId, depositUTxO :: [UTxOType tx], snapshotUTxO :: Maybe (UTxOType tx)}
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (ServerOutput tx)
deriving stock instance IsChainState tx => Show (ServerOutput tx)

instance IsChainState tx => ToJSON (ServerOutput tx) where
  toJSON =
    genericToJSON
      defaultOptions
        { omitNothingFields = True
        }

instance IsChainState tx => FromJSON (ServerOutput tx) where
  parseJSON =
    genericParseJSON
      defaultOptions
        { omitNothingFields = True
        }

instance (ArbitraryIsTx tx, IsChainState tx) => Arbitrary (ServerOutput tx) where
  arbitrary = genericArbitrary

  -- NOTE: Somehow, can't use 'genericShrink' here as GHC is complaining about
  -- Overlapping instances with 'UTxOType tx' even though for a fixed `tx`, there
  -- should be only one 'UTxOType tx'
  shrink = \case
    PeerConnected p -> PeerConnected <$> shrink p
    PeerDisconnected p -> PeerDisconnected <$> shrink p
    PeerHandshakeFailure rh ov tv -> PeerHandshakeFailure <$> shrink rh <*> shrink ov <*> shrink tv
    HeadIsInitializing headId xs -> HeadIsInitializing <$> shrink headId <*> shrink xs
    Committed headId p u -> Committed <$> shrink headId <*> shrink p <*> shrink u
    HeadIsOpen headId u -> HeadIsOpen <$> shrink headId <*> shrink u
    HeadIsClosed headId s t -> HeadIsClosed <$> shrink headId <*> shrink s <*> shrink t
    HeadIsContested headId sn dl -> HeadIsContested <$> shrink headId <*> shrink sn <*> shrink dl
    ReadyToFanout headId -> ReadyToFanout <$> shrink headId
    HeadIsFinalized headId u -> HeadIsFinalized <$> shrink headId <*> shrink u
    HeadIsAborted headId u -> HeadIsAborted <$> shrink headId <*> shrink u
    CommandFailed i s -> CommandFailed <$> shrink i <*> shrink s
    TxValid headId i tx -> TxValid <$> shrink headId <*> shrink i <*> shrink tx
    TxInvalid headId u tx err -> TxInvalid <$> shrink headId <*> shrink u <*> shrink tx <*> shrink err
    SnapshotConfirmed headId s ms -> SnapshotConfirmed <$> shrink headId <*> shrink s <*> shrink ms
    GetUTxOResponse headId u -> GetUTxOResponse <$> shrink headId <*> shrink u
    InvalidInput r i -> InvalidInput <$> shrink r <*> shrink i
    Greetings me headStatus hydraHeadId snapshotUtxo hydraNodeVersion ->
      Greetings
        <$> shrink me
        <*> shrink headStatus
        <*> shrink hydraHeadId
        <*> shrink snapshotUtxo
        <*> shrink hydraNodeVersion
    PostTxOnChainFailed p e -> PostTxOnChainFailed <$> shrink p <*> shrink e
    IgnoredHeadInitializing{} -> []
    DecommitRequested headId txid u -> DecommitRequested headId txid <$> shrink u
    DecommitInvalid headId decommitTx decommitInvalidReason -> DecommitInvalid headId <$> shrink decommitTx <*> shrink decommitInvalidReason
    DecommitApproved headId txid u -> DecommitApproved headId txid <$> shrink u
    DecommitFinalized headId decommitTxId -> DecommitFinalized headId <$> shrink decommitTxId
    CommitRecorded headId u i d -> CommitRecorded headId <$> shrink u <*> shrink i <*> shrink d
    CommitApproved headId u -> CommitApproved headId <$> shrink u
    CommitRecovered headId u rid -> CommitRecovered headId <$> shrink u <*> shrink rid
    CommitFinalized headId theDeposit -> CommitFinalized headId <$> shrink theDeposit
    CommitIgnored headId depositUTxO snapshotUTxO -> CommitIgnored headId <$> shrink depositUTxO <*> shrink snapshotUTxO

instance (ArbitraryIsTx tx, IsChainState tx) => ToADTArbitrary (ServerOutput tx)

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
prepareServerOutput ServerOutputConfig{utxoInSnapshot} response =
  case output response of
    PeerConnected{} -> encodedResponse
    PeerDisconnected{} -> encodedResponse
    PeerHandshakeFailure{} -> encodedResponse
    HeadIsInitializing{} -> encodedResponse
    Committed{} -> encodedResponse
    HeadIsOpen{} -> encodedResponse
    HeadIsClosed{} -> encodedResponse
    HeadIsContested{} -> encodedResponse
    ReadyToFanout{} -> encodedResponse
    HeadIsAborted{} -> encodedResponse
    HeadIsFinalized{} -> encodedResponse
    CommandFailed{} -> encodedResponse
    TxValid{} -> encodedResponse
    TxInvalid{} -> encodedResponse
    SnapshotConfirmed{} ->
      handleUtxoInclusion (key "snapshot" . atKey "utxo" .~ Nothing) encodedResponse
    GetUTxOResponse{} -> encodedResponse
    InvalidInput{} -> encodedResponse
    Greetings{} -> encodedResponse
    PostTxOnChainFailed{} -> encodedResponse
    IgnoredHeadInitializing{} -> encodedResponse
    DecommitRequested{} -> encodedResponse
    DecommitApproved{} -> encodedResponse
    DecommitFinalized{} -> encodedResponse
    DecommitInvalid{} -> encodedResponse
    CommitRecorded{} -> encodedResponse
    CommitApproved{} -> encodedResponse
    CommitFinalized{} -> encodedResponse
    CommitRecovered{} -> encodedResponse
    CommitIgnored{} -> encodedResponse
 where
  handleUtxoInclusion f bs =
    case utxoInSnapshot of
      WithUTxO -> bs
      WithoutUTxO -> bs & f

  encodedResponse = encode response

-- | All information needed to distinguish behavior of the commit endpoint.
data CommitInfo
  = CannotCommit
  | NormalCommit HeadId
  | IncrementalCommit HeadId

--

-- | Projection to obtain the list of pending deposits.
projectPendingDeposits :: IsTx tx => [TxIdType tx] -> ServerOutput tx -> [TxIdType tx]
projectPendingDeposits txIds = \case
  CommitRecorded{pendingDeposit} -> pendingDeposit : txIds
  CommitRecovered{recoveredTxId} -> filter (/= recoveredTxId) txIds
  CommitFinalized{theDeposit} -> filter (/= theDeposit) txIds
  _other -> txIds

-- | Projection to obtain 'CommitInfo' needed to draft commit transactions.
-- NOTE: We only want to project 'HeadId' when the Head is in the 'Initializing'
-- state since this is when Head parties need to commit some funds.
projectCommitInfo :: CommitInfo -> ServerOutput tx -> CommitInfo
projectCommitInfo commitInfo = \case
  HeadIsInitializing{headId} -> NormalCommit headId
  HeadIsOpen{headId} -> IncrementalCommit headId
  HeadIsAborted{} -> CannotCommit
  HeadIsClosed{} -> CannotCommit
  _other -> commitInfo

-- | Projection to obtain the 'HeadId' needed to draft a commit transaction.
-- NOTE: We only want to project 'HeadId' when the Head is in the 'Initializing'
-- state since this is when Head parties need to commit some funds.
projectInitializingHeadId :: Maybe HeadId -> ServerOutput tx -> Maybe HeadId
projectInitializingHeadId mHeadId = \case
  HeadIsInitializing{headId} -> Just headId
  HeadIsOpen{} -> Nothing
  HeadIsAborted{} -> Nothing
  _other -> mHeadId

-- | Projection function related to 'headStatus' field in 'Greetings' message.
projectHeadStatus :: StateChanged.HeadStatus -> ServerOutput tx -> StateChanged.HeadStatus
projectHeadStatus headStatus = \case
  HeadIsInitializing{} -> StateChanged.Initializing
  HeadIsOpen{} -> StateChanged.Open
  HeadIsClosed{} -> StateChanged.Closed
  ReadyToFanout{} -> StateChanged.FanoutPossible
  HeadIsFinalized{} -> StateChanged.Final
  _other -> headStatus

-- | Projection of latest confirmed snapshot UTxO.
projectSnapshotUtxo :: Maybe (UTxOType tx) -> ServerOutput tx -> Maybe (UTxOType tx)
projectSnapshotUtxo snapshotUtxo = \case
  SnapshotConfirmed _ snapshot _ -> Just $ Tx.utxo snapshot
  HeadIsOpen _ utxos -> Just utxos
  _other -> snapshotUtxo

mapStateChangedToServerOutput :: StateChanged.StateChanged tx -> Maybe (ServerOutput tx)
mapStateChangedToServerOutput = \case
  StateChanged.PeerConnected{peer} -> Just $ PeerConnected{peer}
  StateChanged.PeerDisconnected{peer} -> Just $ PeerDisconnected{peer}
  StateChanged.PeerHandshakeFailure{remoteHost, ourVersion, theirVersions} ->
    Just $
      PeerHandshakeFailure{remoteHost, ourVersion, theirVersions}
  StateChanged.HeadIsInitializing{headId, parties} -> Just $ HeadIsInitializing{headId, parties}
  StateChanged.Committed{headId, party, utxo} -> Just $ Committed{headId, party, utxo}
  StateChanged.HeadIsOpen{headId, utxo} -> Just $ HeadIsOpen{headId, utxo}
  StateChanged.HeadIsClosed{headId, snapshotNumber, contestationDeadline} ->
    Just $
      HeadIsClosed{headId, snapshotNumber, contestationDeadline}
  StateChanged.HeadIsContested{headId, snapshotNumber, contestationDeadline} ->
    Just $
      HeadIsContested{headId, snapshotNumber, contestationDeadline}
  StateChanged.ReadyToFanout{headId} -> Just $ ReadyToFanout{headId}
  StateChanged.HeadIsAborted{headId, utxo} -> Just $ HeadIsAborted{headId, utxo}
  StateChanged.HeadIsFinalized{headId, utxo} -> Just $ HeadIsFinalized{headId, utxo}
  StateChanged.CommandFailed{clientInput, state} -> Just $ CommandFailed{clientInput, state}
  StateChanged.TxValid{headId, transactionId, transaction} -> Just $ TxValid{headId, transactionId, transaction}
  StateChanged.TxInvalid{headId, utxo, transaction, validationError} -> Just $ TxInvalid{headId, utxo, transaction, validationError}
  StateChanged.SnapshotConfirmed{headId, snapshot, signatures} -> Just $ SnapshotConfirmed{headId, snapshot, signatures}
  StateChanged.GetUTxOResponse{headId, utxo} -> Just $ GetUTxOResponse{headId, utxo}
  StateChanged.InvalidInput{reason, input} -> Just $ InvalidInput{reason, input}
  StateChanged.Greetings{me, headStatus, hydraHeadId, snapshotUtxo, hydraNodeVersion} ->
    Just $
      Greetings{me, headStatus, hydraHeadId, snapshotUtxo, hydraNodeVersion}
  StateChanged.PostTxOnChainFailed{postChainTx, postTxError} -> Just $ PostTxOnChainFailed{postChainTx, postTxError}
  StateChanged.IgnoredHeadInitializing{headId, contestationPeriod, parties, participants} ->
    Just $
      IgnoredHeadInitializing{headId, contestationPeriod, parties, participants}
  StateChanged.DecommitRequested{headId, decommitTx, utxoToDecommit} -> Just $ DecommitRequested{headId, decommitTx, utxoToDecommit}
  StateChanged.DecommitInvalid{headId, decommitTx, decommitInvalidReason} -> Just $ DecommitInvalid{headId, decommitTx, decommitInvalidReason}
  StateChanged.DecommitApproved{headId, decommitTxId, utxoToDecommit} -> Just $ DecommitApproved{headId, decommitTxId, utxoToDecommit}
  StateChanged.DecommitFinalized{headId, decommitTxId} -> Just $ DecommitFinalized{headId, decommitTxId}
  StateChanged.CommitRecorded{headId, utxoToCommit, pendingDeposit, deadline} ->
    Just $
      CommitRecorded{headId, utxoToCommit, pendingDeposit, deadline}
  StateChanged.CommitApproved{headId, utxoToCommit} -> Just $ CommitApproved{headId, utxoToCommit}
  StateChanged.CommitFinalized{headId, depositTxId} -> Just $ CommitFinalized{headId, theDeposit = depositTxId}
  StateChanged.CommitRecovered{headId, recoveredUTxO, recoveredTxId} ->
    Just $
      CommitRecovered{headId, recoveredUTxO, recoveredTxId}
  StateChanged.CommitIgnored{headId, depositUTxO, snapshotUTxO} -> Just $ CommitIgnored{headId, depositUTxO, snapshotUTxO}
  StateChanged.TransactionReceived{} -> Nothing
  StateChanged.DecommitRecorded{} -> Nothing
  StateChanged.SnapshotRequested{} -> Nothing
  StateChanged.SnapshotRequestDecided{} -> Nothing
  StateChanged.PartySignedSnapshot{} -> Nothing
  StateChanged.ChainRolledBack{} -> Nothing
  StateChanged.TickObserved{} -> Nothing
