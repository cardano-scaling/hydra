{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ServerOutput where

import Control.Lens ((.~))
import Data.Aeson (Value (..), encode, withObject, (.:))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (atKey, key)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Outcome (HeadStatus (..), StateChanged (..))
import Hydra.Prelude hiding (seq)
import Hydra.Tx (
  HeadId,
  IsTx,
 )
import Hydra.Tx qualified as Tx
import Hydra.Tx.IsTx (IsTx (..))

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
    TransactionReceived{} -> encodedResponse
    DecommitRecorded{} -> encodedResponse
    SnapshotRequestDecided{} -> encodedResponse
    SnapshotRequested{} -> encodedResponse
    PartySignedSnapshot{} -> encodedResponse
    ChainRolledBack{} -> encodedResponse
    TickObserved{} -> encodedResponse
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
  HeadIsInitializing{headId} -> NormalCommit headId
  HeadIsOpen{headId} -> IncrementalCommit headId
  HeadIsAborted{} -> CannotCommit
  HeadIsClosed{} -> CannotCommit
  _other -> commitInfo

-- | Projection to obtain the 'HeadId' needed to draft a commit transaction.
-- NOTE: We only want to project 'HeadId' when the Head is in the 'Initializing'
-- state since this is when Head parties need to commit some funds.
projectInitializingHeadId :: Maybe HeadId -> StateChanged tx -> Maybe HeadId
projectInitializingHeadId mHeadId = \case
  HeadIsInitializing{headId} -> Just headId
  HeadIsOpen{} -> Nothing
  HeadIsAborted{} -> Nothing
  _other -> mHeadId

-- | Projection function related to 'headStatus' field in 'Greetings' message.
projectHeadStatus :: HeadStatus -> StateChanged tx -> HeadStatus
projectHeadStatus headStatus = \case
  HeadIsInitializing{} -> Initializing
  HeadIsOpen{} -> Open
  HeadIsClosed{} -> Closed
  ReadyToFanout{} -> FanoutPossible
  HeadIsFinalized{} -> Final
  _other -> headStatus

-- | Projection of latest confirmed snapshot UTxO.
projectSnapshotUtxo :: Maybe (UTxOType tx) -> StateChanged tx -> Maybe (UTxOType tx)
projectSnapshotUtxo snapshotUtxo = \case
  SnapshotConfirmed _ snapshot _ -> Just $ Tx.utxo snapshot
  HeadIsOpen _ _ utxos -> Just utxos
  _other -> snapshotUtxo
