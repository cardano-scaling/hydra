{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ServerOutput where

import Cardano.Binary (serialize')
import Control.Lens ((.~))
import Data.Aeson (Value (..), defaultOptions, encode, genericParseJSON, genericToJSON, omitNothingFields, withObject, (.:))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Lens (atKey, key)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.Chain (ChainStateType, HeadId, IsChainState, PostChainTx (..), PostTxError)
import Hydra.Crypto (MultiSignature)
import Hydra.Ledger (ChainSlot, IsTx, UTxOType, ValidationError)
import Hydra.Network (NodeId)
import Hydra.Party (Party)
import Hydra.Prelude hiding (seq)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (utxo), SnapshotNumber, confirmed)

-- | The type of messages sent to clients by the 'Hydra.API.Server'.
data TimedServerOutput tx = TimedServerOutput
  { output :: ServerOutput tx
  , seq :: Natural
  , time :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance Arbitrary (ServerOutput tx) => Arbitrary (TimedServerOutput tx) where
  arbitrary = genericArbitrary

instance (ToJSON tx, IsChainState tx) => ToJSON (TimedServerOutput tx) where
  toJSON TimedServerOutput{output, seq, time} =
    case toJSON output of
      Object o ->
        Object $ o <> KeyMap.fromList [("seq", toJSON seq), ("timestamp", toJSON time)]
      _NotAnObject -> error "expected ServerOutput to serialize to an Object"

instance (FromJSON tx, IsChainState tx) => FromJSON (TimedServerOutput tx) where
  parseJSON v = flip (withObject "TimedServerOutput") v $ \o ->
    TimedServerOutput <$> parseJSON v <*> o .: "seq" <*> o .: "timestamp"

-- | Individual server output messages as produced by the 'Hydra.HeadLogic' in
-- the 'ClientEffect'.
data ServerOutput tx
  = PeerConnected {peer :: NodeId}
  | PeerDisconnected {peer :: NodeId}
  | HeadIsInitializing {headId :: HeadId, parties :: Set Party}
  | Committed {headId :: HeadId, party :: Party, utxo :: UTxOType tx}
  | HeadIsOpen
      { headId :: HeadId
      , utxo :: UTxOType tx
      , chainSlot :: ChainSlot
      , chainTime :: UTCTime
      , slotLength :: Integer
      }
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
  | HeadIsContested {headId :: HeadId, snapshotNumber :: SnapshotNumber}
  | ReadyToFanout {headId :: HeadId}
  | HeadIsAborted {headId :: HeadId, utxo :: UTxOType tx}
  | HeadIsFinalized {headId :: HeadId, utxo :: UTxOType tx}
  | CommandFailed {clientInput :: ClientInput tx}
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
    Greetings
      { me :: Party
      , headStatus :: HeadStatus
      , snapshotUtxo :: Maybe (UTxOType tx)
      , chainSlot :: ChainSlot
      , systemStart :: UTCTime
      , chainTime :: UTCTime
      , slotLength :: Integer
      }
  | PostTxOnChainFailed {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
  | RolledBack
  deriving (Generic)

deriving instance (IsTx tx, IsChainState tx) => Eq (ServerOutput tx)
deriving instance (IsTx tx, IsChainState tx) => Show (ServerOutput tx)

instance (IsTx tx, IsChainState tx) => ToJSON (ServerOutput tx) where
  toJSON =
    genericToJSON
      defaultOptions
        { omitNothingFields = True
        }

instance (IsTx tx, IsChainState tx) => FromJSON (ServerOutput tx) where
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

  -- NOTE: See note on 'Arbitrary (ClientInput tx)'
  shrink = \case
    PeerConnected p -> PeerConnected <$> shrink p
    PeerDisconnected p -> PeerDisconnected <$> shrink p
    HeadIsInitializing headId xs -> HeadIsInitializing <$> shrink headId <*> shrink xs
    Committed headId p u -> Committed <$> shrink headId <*> shrink p <*> shrink u
    HeadIsOpen headId u chainSlot chainTime slotLength ->
      HeadIsOpen
        <$> shrink headId
        <*> shrink u
        <*> shrink chainSlot
        <*> shrink chainTime
        <*> shrink slotLength
    HeadIsClosed headId s t -> HeadIsClosed <$> shrink headId <*> shrink s <*> shrink t
    HeadIsContested headId sn -> HeadIsContested <$> shrink headId <*> shrink sn
    ReadyToFanout headId -> ReadyToFanout <$> shrink headId
    HeadIsFinalized headId u -> HeadIsFinalized <$> shrink headId <*> shrink u
    HeadIsAborted headId u -> HeadIsAborted <$> shrink headId <*> shrink u
    CommandFailed i -> CommandFailed <$> shrink i
    TxValid headId tx -> TxValid <$> shrink headId <*> shrink tx
    TxInvalid headId u tx err -> TxInvalid <$> shrink headId <*> shrink u <*> shrink tx <*> shrink err
    SnapshotConfirmed headId s ms -> SnapshotConfirmed <$> shrink headId <*> shrink s <*> shrink ms
    GetUTxOResponse headId u -> GetUTxOResponse <$> shrink headId <*> shrink u
    InvalidInput r i -> InvalidInput <$> shrink r <*> shrink i
    Greetings me headStatus snapshotUtxo chainSlot sytemStart chainTime slotLength ->
      Greetings
        <$> shrink me
        <*> shrink headStatus
        <*> shrink snapshotUtxo
        <*> shrink chainSlot
        <*> shrink sytemStart
        <*> shrink chainTime
        <*> shrink slotLength
    PostTxOnChainFailed p e -> PostTxOnChainFailed <$> shrink p <*> shrink e
    RolledBack -> []

-- | Possible transaction formats in the api server output
data OutputFormat
  = OutputCBOR
  | OutputJSON
  deriving (Eq, Show)

-- | Whether or not to include full UTxO in server outputs.
data WithUTxO = WithUTxO | WithoutUTxO
  deriving (Eq, Show)

data ServerOutputConfig = ServerOutputConfig
  { txOutputFormat :: OutputFormat
  , utxoInSnapshot :: WithUTxO
  }
  deriving (Eq, Show)

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
prepareServerOutput ServerOutputConfig{txOutputFormat, utxoInSnapshot} response =
  case output response of
    PeerConnected{} -> encodedResponse
    PeerDisconnected{} -> encodedResponse
    HeadIsInitializing{} -> encodedResponse
    Committed{} -> encodedResponse
    HeadIsOpen{} -> encodedResponse
    HeadIsClosed{} -> encodedResponse
    HeadIsContested{} -> encodedResponse
    ReadyToFanout{} -> encodedResponse
    HeadIsAborted{} -> encodedResponse
    HeadIsFinalized{} -> encodedResponse
    CommandFailed{clientInput} ->
      case clientInput of
        Init -> encodedResponse
        Abort -> encodedResponse
        Commit{} -> encodedResponse
        NewTx{Hydra.API.ClientInput.transaction = tx} ->
          handleTxOutput
            (key "transaction" .~ txToCbor tx)
            encodedResponse
        GetUTxO -> encodedResponse
        Close -> encodedResponse
        Contest -> encodedResponse
        Fanout -> encodedResponse
    TxValid{Hydra.API.ServerOutput.transaction = tx} ->
      handleTxOutput
        (key "transaction" .~ txToCbor tx)
        encodedResponse
    TxInvalid{Hydra.API.ServerOutput.transaction = tx} ->
      handleTxOutput
        (key "transaction" .~ txToCbor tx)
        encodedResponse
    SnapshotConfirmed{Hydra.API.ServerOutput.snapshot} ->
      handleUtxoInclusion (key "snapshot" . atKey "utxo" .~ Nothing) $
        handleTxOutput
          ( key "snapshot"
              . key "confirmedTransactions"
              .~ toJSON (txToCbor <$> confirmed snapshot)
          )
          encodedResponse
    GetUTxOResponse{} -> encodedResponse
    InvalidInput{} -> encodedResponse
    Greetings{} -> encodedResponse
    PostTxOnChainFailed{postChainTx} ->
      case postChainTx of
        CloseTx{confirmedSnapshot} ->
          case confirmedSnapshot of
            InitialSnapshot{} -> encodedResponse
            ConfirmedSnapshot{Hydra.Snapshot.snapshot} ->
              handleTxOutput
                ( key "postChainTx"
                    . key "confirmedSnapshot"
                    . key "snapshot"
                    . key "confirmedTransactions"
                    .~ toJSON (txToCbor <$> confirmed snapshot)
                )
                encodedResponse
        ContestTx{confirmedSnapshot} ->
          case confirmedSnapshot of
            InitialSnapshot{} -> encodedResponse
            ConfirmedSnapshot{Hydra.Snapshot.snapshot} ->
              handleTxOutput
                ( key "postChainTx"
                    . key "confirmedSnapshot"
                    . key "snapshot"
                    . key "confirmedTransactions"
                    .~ toJSON (txToCbor <$> confirmed snapshot)
                )
                encodedResponse
        _other -> encodedResponse
    RolledBack -> encodedResponse
 where
  handleUtxoInclusion f bs =
    case utxoInSnapshot of
      WithUTxO -> bs
      WithoutUTxO -> bs & f

  handleTxOutput f bs =
    case txOutputFormat of
      OutputJSON -> bs
      OutputCBOR -> bs & f

  encodedResponse = encode response

  txToCbor =
    String . decodeUtf8 . Base16.encode . serialize'

-- | All possible Hydra states displayed in the API server outputs.
data HeadStatus
  = Idle
  | Initializing
  | Open
  | Closed
  | FanoutPossible
  | Final
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Arbitrary HeadStatus where
  arbitrary = genericArbitrary

-- | Projection function related to 'headStatus' field in 'Greetings' message.
projectHeadStatus :: HeadStatus -> ServerOutput tx -> HeadStatus
projectHeadStatus headStatus = \case
  HeadIsInitializing{} -> Initializing
  HeadIsOpen{} -> Open
  HeadIsClosed{} -> Closed
  ReadyToFanout{} -> FanoutPossible
  HeadIsFinalized{} -> Final
  _other -> headStatus

-- | Projection function related to 'snapshotUtxo' field in 'Greetings' message.
projectSnapshotUtxo :: Maybe (UTxOType tx) -> ServerOutput tx -> Maybe (UTxOType tx)
projectSnapshotUtxo snapshotUtxo = \case
  SnapshotConfirmed _ snapshot _ -> Just $ Hydra.Snapshot.utxo snapshot
  HeadIsOpen _ utxos _ _ _ -> Just utxos
  _other -> snapshotUtxo
