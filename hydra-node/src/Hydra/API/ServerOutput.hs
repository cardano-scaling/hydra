{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ServerOutput where

import Control.Lens ((.~))
import Data.Aeson (Value (..), defaultOptions, encode, genericParseJSON, genericToJSON, withObject, (.:))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (atKey, key)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.Chain (PostChainTx, PostTxError)
import Hydra.Chain.ChainState (ChainSlot, IsChainState (ChainStateType))
import Hydra.HeadLogic.State (HeadState)
import Hydra.Ledger (ValidationError)
import Hydra.Network (Host, NodeId)
import Hydra.Prelude hiding (seq)
import Hydra.Tx (
  HeadId,
  IsTx,
  Party,
  Snapshot,
  SnapshotNumber,
  SnapshotVersion,
  mkHeadParameters,
 )
import Hydra.Tx qualified as Tx
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Crypto (MultiSignature, Signature)
import Hydra.Tx.Environment (Environment (..))
import Hydra.Tx.IsTx (ArbitraryIsTx, IsTx (..))
import Hydra.Tx.OnChainId (OnChainId)
import Test.QuickCheck (oneof)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

-- | Head state changed event. These events represent all the internal state
-- changes, get persisted and processed in an event sourcing manner.
data StateChanged tx
  = PeerConnected {peer :: NodeId}
  | PeerDisconnected {peer :: NodeId}
  | PeerHandshakeFailure
      { remoteHost :: Host
      , ourVersion :: Natural
      , theirVersions :: [Natural]
      }
  | HeadInitialized
      { headId :: HeadId
      , parameters :: Tx.HeadParameters
      , chainState :: ChainStateType tx
      , headSeed :: Tx.HeadSeed
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
      { tx :: tx
      , newLocalUTxO :: UTxOType tx
      }
  | -- | Given transaction was not not applicable to the given UTxO in time and
    -- has been dropped.
    TxInvalid {headId :: HeadId, utxo :: UTxOType tx, transaction :: tx, validationError :: ValidationError}
  | CommitRecorded {headId :: HeadId, pendingDeposit :: Map (TxIdType tx) (UTxOType tx), newLocalUTxO :: UTxOType tx}
  | CommitRecovered {recoveredUTxO :: UTxOType tx, newLocalUTxO :: UTxOType tx, recoveredTxId :: TxIdType tx}
  | DecommitRecorded {decommitTx :: tx, newLocalUTxO :: UTxOType tx}
  | SnapshotRequestDecided {snapshotNumber :: SnapshotNumber}
  | -- | A snapshot was requested by some party.
    -- NOTE: We deliberately already include an updated local ledger state to
    -- not need a ledger to interpret this event.
    SnapshotRequested
      { snapshot :: Snapshot tx
      , requestedTxIds :: [TxIdType tx]
      , newLocalUTxO :: UTxOType tx
      , newLocalTxs :: [tx]
      }
  | CommitApproved {headId :: HeadId, utxoToCommit :: UTxOType tx}
  | CommitIgnored {headId :: HeadId, depositUTxO :: [UTxOType tx], snapshotUTxO :: Maybe (UTxOType tx)}
  | CommitFinalized {headId :: HeadId, newVersion :: SnapshotVersion, depositTxId :: TxIdType tx}
  | DecommitRequested {headId :: HeadId, decommitTx :: tx, utxoToDecommit :: UTxOType tx}
  | DecommitInvalid {headId :: HeadId, decommitTx :: tx, decommitInvalidReason :: DecommitInvalidReason tx}
  | DecommitApproved {headId :: HeadId, decommitTxId :: TxIdType tx, utxoToDecommit :: UTxOType tx}
  | DecommitFinalized {newVersion :: SnapshotVersion}
  | PartySignedSnapshot {snapshot :: Snapshot tx, party :: Party, signature :: Signature (Snapshot tx)}
  | SnapshotConfirmed {headId :: HeadId, snapshot :: Snapshot tx, signatures :: MultiSignature (Snapshot tx)}
  | HeadClosed {chainState :: ChainStateType tx, contestationDeadline :: UTCTime}
  | HeadContested {chainState :: ChainStateType tx, contestationDeadline :: UTCTime}
  | HeadIsReadyToFanout
  | HeadFannedOut {headId :: HeadId, chainState :: ChainStateType tx}
  | ChainRolledBack {chainState :: ChainStateType tx}
  | TickObserved {chainSlot :: ChainSlot}
  | CommandFailed {clientInput :: ClientInput tx, state :: HeadState tx}
  | GetUTxOResponse {headId :: HeadId, utxo :: UTxOType tx}
  | InvalidInput {reason :: String, input :: Text}
  | -- | A friendly welcome message which tells a client something about the
    -- node. Currently used for knowing what signing key the server uses (it
    -- only knows one), 'HeadStatus' and optionally (if 'HeadIsOpen' or
    -- 'SnapshotConfirmed' message is emitted) UTxO's present in the Hydra Head.
    Greetings
      { me :: Party
      , headStatus :: HeadStatus
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
  deriving stock (Generic)

deriving stock instance (IsTx tx, IsChainState tx, Eq (HeadState tx), Eq (ChainStateType tx)) => Eq (StateChanged tx)
deriving stock instance (IsTx tx, IsChainState tx, Show (HeadState tx), Show (ChainStateType tx)) => Show (StateChanged tx)
deriving anyclass instance (IsTx tx, IsChainState tx, ToJSON (ChainStateType tx)) => ToJSON (StateChanged tx)
deriving anyclass instance (IsTx tx, IsChainState tx, FromJSON (HeadState tx), FromJSON (ChainStateType tx)) => FromJSON (StateChanged tx)

genStateChanged :: (ArbitraryIsTx tx, IsChainState tx) => Environment -> Gen (StateChanged tx)
genStateChanged env =
  oneof
    [ HeadInitialized <$> arbitrary <*> pure (mkHeadParameters env) <*> arbitrary <*> arbitrary
    , CommittedUTxO <$> arbitrary <*> pure party <*> arbitrary <*> arbitrary
    , HeadAborted <$> arbitrary <*> arbitrary <*> arbitrary
    , HeadOpened <$> arbitrary <*> arbitrary <*> arbitrary
    , TransactionReceived <$> arbitrary
    , TransactionAppliedToLocalUTxO <$> arbitrary <*> arbitrary
    , DecommitRecorded <$> arbitrary <*> arbitrary
    , SnapshotRequestDecided <$> arbitrary
    , SnapshotRequested <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , PartySignedSnapshot <$> arbitrary <*> arbitrary <*> arbitrary
    , SnapshotConfirmed <$> arbitrary <*> arbitrary <*> arbitrary
    , DecommitFinalized <$> arbitrary
    , HeadClosed <$> arbitrary <*> arbitrary
    , HeadContested <$> arbitrary <*> arbitrary
    , pure HeadIsReadyToFanout
    , HeadFannedOut <$> arbitrary <*> arbitrary
    , ChainRolledBack <$> arbitrary
    , TickObserved <$> arbitrary
    ]
 where
  Environment{party} = env

instance (ArbitraryIsTx tx, IsChainState tx) => Arbitrary (StateChanged tx) where
  arbitrary = arbitrary >>= genStateChanged

instance (ArbitraryIsTx tx, IsChainState tx) => ToADTArbitrary (StateChanged tx)

-- | The type of messages sent to clients by the 'Hydra.API.Server'.
data TimedServerOutput tx = TimedServerOutput
  { output :: StateChanged tx
  , seq :: Natural
  , time :: UTCTime
  }
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (TimedServerOutput tx)
deriving stock instance IsChainState tx => Show (TimedServerOutput tx)

instance Arbitrary (StateChanged tx) => Arbitrary (TimedServerOutput tx) where
  arbitrary = genericArbitrary

-- | Generate a random timed server output given a normal server output.
genTimedServerOutput :: StateChanged tx -> Gen (TimedServerOutput tx)
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

instance ArbitraryIsTx tx => Arbitrary (DecommitInvalidReason tx) where
  arbitrary = genericArbitrary

-- -- | Individual server output messages as produced by the 'Hydra.HeadLogic' in
-- -- the 'ClientEffect'.
-- data ServerOutput tx
--   = PeerConnected {peer :: NodeId}
--   | PeerDisconnected {peer :: NodeId}
--   | PeerHandshakeFailure
--       { remoteHost :: Host
--       , ourVersion :: Natural
--       , theirVersions :: [Natural]
--       }
--   | HeadIsInitializing {headId :: HeadId, parties :: [Party]}
--   | Committed {headId :: HeadId, party :: Party, utxo :: UTxOType tx}
--   | HeadIsOpen {headId :: HeadId, utxo :: UTxOType tx}
--   | HeadIsClosed
--       { headId :: HeadId
--       , snapshotNumber :: SnapshotNumber
--       , contestationDeadline :: UTCTime
--       -- ^ Nominal deadline until which contest can be submitted and after
--       -- which fanout is possible. NOTE: Use this only for informational
--       -- purpose and wait for 'ReadyToFanout' instead before sending 'Fanout'
--       -- as the ledger of our cardano-node might not have progressed
--       -- sufficiently in time yet and we do not re-submit transactions (yet).
--       }
--   | HeadIsContested {headId :: HeadId, snapshotNumber :: SnapshotNumber, contestationDeadline :: UTCTime}
--   | ReadyToFanout {headId :: HeadId}
--   | HeadIsAborted {headId :: HeadId, utxo :: UTxOType tx}
--   | HeadIsFinalized {headId :: HeadId, utxo :: UTxOType tx}
--   | CommandFailed {clientInput :: ClientInput tx, state :: HeadState tx}
--   | -- | Given transaction has been seen as valid in the Head. It is expected to
--     -- eventually be part of a 'SnapshotConfirmed'.
--     TxValid {headId :: HeadId, transactionId :: TxIdType tx, transaction :: tx}
--   | -- | Given transaction was not not applicable to the given UTxO in time and
--     -- has been dropped.
--     TxInvalid {headId :: HeadId, utxo :: UTxOType tx, transaction :: tx, validationError :: ValidationError}
--   | -- | Given snapshot was confirmed and included transactions can be
--     -- considered final.
--     SnapshotConfirmed
--       { headId :: HeadId
--       , snapshot :: Snapshot tx
--       , signatures :: MultiSignature (Snapshot tx)
--       }
--   | GetUTxOResponse {headId :: HeadId, utxo :: UTxOType tx}
--   | InvalidInput {reason :: String, input :: Text}
--   | -- | A friendly welcome message which tells a client something about the
--     -- node. Currently used for knowing what signing key the server uses (it
--     -- only knows one), 'HeadStatus' and optionally (if 'HeadIsOpen' or
--     -- 'SnapshotConfirmed' message is emitted) UTxO's present in the Hydra Head.
--     Greetings
--       { me :: Party
--       , headStatus :: HeadStatus
--       , hydraHeadId :: Maybe HeadId
--       , snapshotUtxo :: Maybe (UTxOType tx)
--       , hydraNodeVersion :: String
--       }
--   | PostTxOnChainFailed {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
--   | IgnoredHeadInitializing
--       { headId :: HeadId
--       , contestationPeriod :: ContestationPeriod
--       , parties :: [Party]
--       , participants :: [OnChainId]
--       }
--   | DecommitRequested {headId :: HeadId, decommitTx :: tx, utxoToDecommit :: UTxOType tx}
--   | DecommitInvalid {headId :: HeadId, decommitTx :: tx, decommitInvalidReason :: DecommitInvalidReason tx}
--   | DecommitApproved {headId :: HeadId, decommitTxId :: TxIdType tx, utxoToDecommit :: UTxOType tx}
--   | DecommitFinalized {headId :: HeadId, decommitTxId :: TxIdType tx}
--   | CommitRecorded {headId :: HeadId, utxoToCommit :: UTxOType tx, pendingDeposit :: TxIdType tx, deadline :: UTCTime}
--   | CommitApproved {headId :: HeadId, utxoToCommit :: UTxOType tx}
--   | CommitFinalized {headId :: HeadId, theDeposit :: TxIdType tx}
--   | CommitRecovered {headId :: HeadId, recoveredUTxO :: UTxOType tx, recoveredTxId :: TxIdType tx}
--   | CommitIgnored {headId :: HeadId, depositUTxO :: [UTxOType tx], snapshotUTxO :: Maybe (UTxOType tx)}
--   deriving stock (Generic)
--
-- deriving stock instance IsChainState tx => Eq (ServerOutput tx)
-- deriving stock instance IsChainState tx => Show (ServerOutput tx)
--
-- instance IsChainState tx => ToJSON (ServerOutput tx) where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { omitNothingFields = True
--         }
--
-- instance IsChainState tx => FromJSON (ServerOutput tx) where
--   parseJSON =
--     genericParseJSON
--       defaultOptions
--         { omitNothingFields = True
--         }
--
-- instance (ArbitraryIsTx tx, IsChainState tx) => Arbitrary (ServerOutput tx) where
--   arbitrary = genericArbitrary
--
--   -- NOTE: Somehow, can't use 'genericShrink' here as GHC is complaining about
--   -- Overlapping instances with 'UTxOType tx' even though for a fixed `tx`, there
--   -- should be only one 'UTxOType tx'
--   shrink = \case
--     PeerConnected p -> PeerConnected <$> shrink p
--     PeerDisconnected p -> PeerDisconnected <$> shrink p
--     PeerHandshakeFailure rh ov tv -> PeerHandshakeFailure <$> shrink rh <*> shrink ov <*> shrink tv
--     HeadIsInitializing headId xs -> HeadIsInitializing <$> shrink headId <*> shrink xs
--     Committed headId p u -> Committed <$> shrink headId <*> shrink p <*> shrink u
--     HeadIsOpen headId u -> HeadIsOpen <$> shrink headId <*> shrink u
--     HeadIsClosed headId s t -> HeadIsClosed <$> shrink headId <*> shrink s <*> shrink t
--     HeadIsContested headId sn dl -> HeadIsContested <$> shrink headId <*> shrink sn <*> shrink dl
--     ReadyToFanout headId -> ReadyToFanout <$> shrink headId
--     HeadIsFinalized headId u -> HeadIsFinalized <$> shrink headId <*> shrink u
--     HeadIsAborted headId u -> HeadIsAborted <$> shrink headId <*> shrink u
--     CommandFailed i s -> CommandFailed <$> shrink i <*> shrink s
--     TxValid headId i tx -> TxValid <$> shrink headId <*> shrink i <*> shrink tx
--     TxInvalid headId u tx err -> TxInvalid <$> shrink headId <*> shrink u <*> shrink tx <*> shrink err
--     SnapshotConfirmed headId s ms -> SnapshotConfirmed <$> shrink headId <*> shrink s <*> shrink ms
--     GetUTxOResponse headId u -> GetUTxOResponse <$> shrink headId <*> shrink u
--     InvalidInput r i -> InvalidInput <$> shrink r <*> shrink i
--     Greetings me headStatus hydraHeadId snapshotUtxo hydraNodeVersion ->
--       Greetings
--         <$> shrink me
--         <*> shrink headStatus
--         <*> shrink hydraHeadId
--         <*> shrink snapshotUtxo
--         <*> shrink hydraNodeVersion
--     PostTxOnChainFailed p e -> PostTxOnChainFailed <$> shrink p <*> shrink e
--     IgnoredHeadInitializing{} -> []
--     DecommitRequested headId txid u -> DecommitRequested headId txid <$> shrink u
--     DecommitInvalid headId decommitTx decommitInvalidReason -> DecommitInvalid headId <$> shrink decommitTx <*> shrink decommitInvalidReason
--     DecommitApproved headId txid u -> DecommitApproved headId txid <$> shrink u
--     DecommitFinalized headId decommitTxId -> DecommitFinalized headId <$> shrink decommitTxId
--     CommitRecorded headId u i d -> CommitRecorded headId <$> shrink u <*> shrink i <*> shrink d
--     CommitApproved headId u -> CommitApproved headId <$> shrink u
--     CommitRecovered headId u rid -> CommitRecovered headId <$> shrink u <*> shrink rid
--     CommitFinalized headId theDeposit -> CommitFinalized headId <$> shrink theDeposit
--     CommitIgnored headId depositUTxO snapshotUTxO -> CommitIgnored headId <$> shrink depositUTxO <*> shrink snapshotUTxO
--
-- instance (ArbitraryIsTx tx, IsChainState tx) => ToADTArbitrary (ServerOutput tx)

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
    HeadInitialized{} -> encodedResponse
    CommittedUTxO{} -> encodedResponse
    HeadOpened{} -> encodedResponse
    HeadClosed{} -> encodedResponse
    HeadContested{} -> encodedResponse
    HeadIsReadyToFanout{} -> encodedResponse
    HeadAborted{} -> encodedResponse
    HeadFannedOut{} -> encodedResponse
    CommandFailed{} -> encodedResponse
    TransactionAppliedToLocalUTxO{} -> encodedResponse
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

-- | All possible Hydra states displayed in the API server outputs.
data HeadStatus
  = Idle
  | Initializing
  | Open
  | Closed
  | FanoutPossible
  | Final
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary HeadStatus where
  arbitrary = genericArbitrary

-- | All information needed to distinguish behavior of the commit endpoint.
data CommitInfo
  = CannotCommit
  | NormalCommit HeadId
  | IncrementalCommit HeadId

--

-- | Projection to obtain the list of pending deposits.
projectPendingDeposits :: IsTx tx => [TxIdType tx] -> StateChanged tx -> [TxIdType tx]
projectPendingDeposits txIds = \case
  CommitRecorded{pendingDeposit} -> Map.keys pendingDeposit <> txIds
  CommitRecovered{recoveredTxId} -> filter (/= recoveredTxId) txIds
  CommitFinalized{depositTxId} -> filter (/= depositTxId) txIds
  _other -> txIds

-- | Projection to obtain 'CommitInfo' needed to draft commit transactions.
-- NOTE: We only want to project 'HeadId' when the Head is in the 'Initializing'
-- state since this is when Head parties need to commit some funds.
projectCommitInfo :: CommitInfo -> StateChanged tx -> CommitInfo
projectCommitInfo commitInfo = \case
  HeadInitialized{headId} -> NormalCommit headId
  HeadOpened{headId} -> IncrementalCommit headId
  HeadAborted{} -> CannotCommit
  HeadClosed{} -> CannotCommit
  _other -> commitInfo

-- | Projection to obtain the 'HeadId' needed to draft a commit transaction.
-- NOTE: We only want to project 'HeadId' when the Head is in the 'Initializing'
-- state since this is when Head parties need to commit some funds.
projectInitializingHeadId :: Maybe HeadId -> StateChanged tx -> Maybe HeadId
projectInitializingHeadId mHeadId = \case
  HeadInitialized{headId} -> Just headId
  HeadOpened{} -> Nothing
  HeadAborted{} -> Nothing
  _other -> mHeadId

-- | Projection function related to 'headStatus' field in 'Greetings' message.
projectHeadStatus :: HeadStatus -> StateChanged tx -> HeadStatus
projectHeadStatus headStatus = \case
  HeadInitialized{} -> Initializing
  HeadOpened{} -> Open
  HeadClosed{} -> Closed
  HeadIsReadyToFanout{} -> FanoutPossible
  HeadFannedOut{} -> Final
  _other -> headStatus

-- | Projection of latest confirmed snapshot UTxO.
projectSnapshotUtxo :: Maybe (UTxOType tx) -> StateChanged tx -> Maybe (UTxOType tx)
projectSnapshotUtxo snapshotUtxo = \case
  SnapshotConfirmed _ snapshot _ -> Just $ Tx.utxo snapshot
  HeadOpened _ _ utxos -> Just utxos
  _other -> snapshotUtxo
