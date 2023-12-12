{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ServerOutput where

import Control.Lens ((.~))
import Data.Aeson (Value (..), defaultOptions, encode, genericParseJSON, genericToJSON, omitNothingFields, withObject, (.:))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (atKey, key)
import Data.ByteString.Lazy qualified as LBS
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.Chain (ChainStateType, IsChainState, PostChainTx (..), PostTxError)
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (MultiSignature)
import Hydra.HeadId (HeadId)
import Hydra.HeadLogic.State (HeadState)
import Hydra.Ledger (IsTx, UTxOType, ValidationError)
import Hydra.Network (Host, NodeId)
import Hydra.OnChainId (OnChainId)
import Hydra.Party (Party)
import Hydra.Prelude hiding (seq)
import Hydra.Snapshot (Snapshot (utxo), SnapshotNumber)

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

data DecommitInvalidReason tx
  = DecommitTxInvalid {confirmedUTxO :: UTxOType tx, decommitTx :: tx, validationError :: ValidationError}
  | DecommitAlreadyInFlight {decommitTx :: tx}
  deriving stock (Generic)

deriving stock instance (Eq tx, Eq (UTxOType tx)) => Eq (DecommitInvalidReason tx)
deriving stock instance (Show tx, Show (UTxOType tx)) => Show (DecommitInvalidReason tx)

instance (ToJSON tx, ToJSON (UTxOType tx)) => ToJSON (DecommitInvalidReason tx) where
  toJSON = genericToJSON defaultOptions

instance (FromJSON tx, FromJSON (UTxOType tx)) => FromJSON (DecommitInvalidReason tx) where
  parseJSON = genericParseJSON defaultOptions

instance
  IsTx tx =>
  Arbitrary (DecommitInvalidReason tx)
  where
  arbitrary = genericArbitrary

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
    TxValid {headId :: HeadId, transaction :: tx}
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
    Greetings {me :: Party, headStatus :: HeadStatus, snapshotUtxo :: Maybe (UTxOType tx), hydraNodeVersion :: String}
  | PostTxOnChainFailed {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
  | IgnoredHeadInitializing
      { headId :: HeadId
      , contestationPeriod :: ContestationPeriod
      , parties :: [Party]
      , participants :: [OnChainId]
      }
  | DecommitRequested {headId :: HeadId, utxoToDecommit :: UTxOType tx}
  | DecommitInvalid {headId :: HeadId, decommitInvalidReason :: DecommitInvalidReason tx}
  | DecommitApproved {headId :: HeadId, utxoToDecommit :: UTxOType tx}
  | DecommitFinalized {headId :: HeadId}
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

instance
  ( IsTx tx
  , Arbitrary (ChainStateType tx)
  ) =>
  Arbitrary (ServerOutput tx)
  where
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
    TxValid headId tx -> TxValid <$> shrink headId <*> shrink tx
    TxInvalid headId u tx err -> TxInvalid <$> shrink headId <*> shrink u <*> shrink tx <*> shrink err
    SnapshotConfirmed headId s ms -> SnapshotConfirmed <$> shrink headId <*> shrink s <*> shrink ms
    GetUTxOResponse headId u -> GetUTxOResponse <$> shrink headId <*> shrink u
    InvalidInput r i -> InvalidInput <$> shrink r <*> shrink i
    Greetings me headStatus snapshotUtxo hydraNodeVersion ->
      Greetings
        <$> shrink me
        <*> shrink headStatus
        <*> shrink snapshotUtxo
        <*> shrink hydraNodeVersion
    PostTxOnChainFailed p e -> PostTxOnChainFailed <$> shrink p <*> shrink e
    IgnoredHeadInitializing{} -> []
    DecommitRequested headId u -> DecommitRequested <$> shrink headId <*> shrink u
    DecommitInvalid headId reason -> DecommitInvalid <$> shrink headId <*> shrink reason
    DecommitApproved headId u -> DecommitApproved <$> shrink headId <*> shrink u
    DecommitFinalized headId -> DecommitFinalized <$> shrink headId

-- | Whether or not to include full UTxO in server outputs.
data WithUTxO = WithUTxO | WithoutUTxO
  deriving stock (Eq, Show)

newtype ServerOutputConfig = ServerOutputConfig
  { utxoInSnapshot :: WithUTxO
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
    TxValid{Hydra.API.ServerOutput.transaction = tx} ->
      (key "transaction" .~ toJSON tx) encodedResponse
    TxInvalid{Hydra.API.ServerOutput.transaction = tx} ->
      (key "transaction" .~ toJSON tx) encodedResponse
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
projectHeadStatus :: HeadStatus -> ServerOutput tx -> HeadStatus
projectHeadStatus headStatus = \case
  HeadIsInitializing{} -> Initializing
  HeadIsOpen{} -> Open
  HeadIsClosed{} -> Closed
  ReadyToFanout{} -> FanoutPossible
  HeadIsFinalized{} -> Final
  _other -> headStatus

-- | Projection of latest confirmed snapshot UTxO.
projectSnapshotUtxo :: Maybe (UTxOType tx) -> ServerOutput tx -> Maybe (UTxOType tx)
projectSnapshotUtxo snapshotUtxo = \case
  SnapshotConfirmed _ snapshot _ -> Just $ Hydra.Snapshot.utxo snapshot
  HeadIsOpen _ utxos -> Just utxos
  _other -> snapshotUtxo
