{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ServerOutput where

import Cardano.Binary (serialize')
import Data.Aeson (Value (..), defaultOptions, genericParseJSON, genericToJSON, object, omitNothingFields, withObject, (.:))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Base16 as Base16
import Data.Reflection
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.Chain (ChainStateType, HeadId, IsChainState, PostChainTx (..), PostTxError)
import Hydra.Crypto (MultiSignature)
import Hydra.Ledger (IsTx, UTxOType, ValidationError)
import Hydra.Network (NodeId)
import Hydra.Party (Party)
import Hydra.Prelude hiding (seq)
import Hydra.Snapshot (Snapshot (Snapshot, confirmed, number, utxo), SnapshotNumber)

-- | The type of messages sent to clients by the 'Hydra.API.Server'.
data TimedServerOutput r tx = TimedServerOutput
  { output :: ServerOutput r tx
  , seq :: Natural
  , time :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance Arbitrary (ServerOutput r tx) => Arbitrary (TimedServerOutput r tx) where
  arbitrary = genericArbitrary

instance (ToJSON tx, IsChainState tx, Reifies r ServerOutputConfig) => ToJSON (TimedServerOutput r tx) where
  toJSON TimedServerOutput{output, seq, time} =
    case toJSON output of
      Object o ->
        Object $ o <> KeyMap.fromList [("seq", toJSON seq), ("timestamp", toJSON time)]
      _NotAnObject -> error "expected ServerOutput to serialize to an Object"

instance (FromJSON tx, IsChainState tx) => FromJSON (TimedServerOutput r tx) where
  parseJSON v = flip (withObject "TimedServerOutput") v $ \o ->
    TimedServerOutput <$> parseJSON v <*> o .: "seq" <*> o .: "timestamp"

-- | Individual server output messages as produced by the 'Hydra.HeadLogic' in
-- the 'ClientEffect'.
data ServerOutput r tx
  = PeerConnected {peer :: NodeId}
  | PeerDisconnected {peer :: NodeId}
  | HeadIsInitializing {headId :: HeadId, parties :: Set Party}
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
    Greetings {me :: Party, headStatus :: HeadStatus, snapshotUtxo :: Maybe (UTxOType tx)}
  | PostTxOnChainFailed {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
  deriving (Generic)

deriving instance (IsTx tx, IsChainState tx) => Eq (ServerOutput r tx)
deriving instance (IsTx tx, IsChainState tx) => Show (ServerOutput r tx)

instance (IsTx tx, IsChainState tx, Reifies r ServerOutputConfig) => ToJSON (ServerOutput r tx) where
  toJSON so =
    case so of
      commandFailed@(CommandFailed clientInput) ->
        case clientInput of
          NewTx{Hydra.API.ClientInput.transaction = tx} ->
            handleTxOutput
              commandFailed
              ( object
                  [
                    ( "NewTx"
                    , object
                        [ ("transaction", txToCbor tx)
                        ]
                    )
                  ]
              )
          _ -> defGeneric commandFailed
      txValid@(TxValid headId tx) ->
        handleTxOutput
          txValid
          ( object
              [
                ( "TxValid"
                , object
                    [ ("headId", toJSON headId)
                    , ("transaction", txToCbor tx)
                    ]
                )
              ]
          )
      txInvalid@TxInvalid{headId, Hydra.API.ServerOutput.utxo, Hydra.API.ServerOutput.transaction = tx, validationError} ->
        handleTxOutput
          txInvalid
          ( object
              [
                ( "TxInvalid"
                , object
                    [ ("headId", toJSON headId)
                    , ("utxo", toJSON utxo)
                    , ("transaction", txToCbor tx)
                    , ("validationError", toJSON validationError)
                    ]
                )
              ]
          )
      snapshotConfirmed@SnapshotConfirmed
        { headId
        , Hydra.API.ServerOutput.snapshot =
          Snapshot{number, Hydra.Snapshot.utxo, confirmed}
        , Hydra.API.ServerOutput.signatures
        } ->
          handleUtxoInclusion (fromText "utxo") $
            handleTxOutput
              snapshotConfirmed
              ( object
                  [
                    ( "SnapshotConfirmed"
                    , object
                        [ ("headId", toJSON headId)
                        ,
                          ( "snapshot"
                          , object
                              [ ("number", toJSON number)
                              , ("utxo", toJSON utxo)
                              , ("confirmed", toJSON $ txToCbor <$> confirmed)
                              ]
                          )
                        , ("signatures", toJSON signatures)
                        ]
                    )
                  ]
              )
      a -> defGeneric a
   where
    handleUtxoInclusion k val =
      case utxoInSnapshot outputConf of
        WithUTxO -> val
        WithoutUTxO ->
          case val of
            Object v -> toJSON $ KeyMap.delete k v
            _ -> val

    handleTxOutput json cbor =
      case txOutputFormat outputConf of
        OutputJSON -> defGeneric json
        OutputCBOR -> cbor

    outputConf = reflect (Proxy :: Proxy r)

    txToCbor = String . decodeUtf8 . Base16.encode . serialize'

    defGeneric =
      genericToJSON
        defaultOptions
          { omitNothingFields = True
          }

instance (IsTx tx, IsChainState tx) => FromJSON (ServerOutput r tx) where
  parseJSON =
    genericParseJSON
      defaultOptions
        { omitNothingFields = True
        }

instance
  ( IsTx tx
  , Arbitrary (ChainStateType tx)
  ) =>
  Arbitrary (ServerOutput r tx)
  where
  arbitrary = genericArbitrary

  -- NOTE: See note on 'Arbitrary (ClientInput tx)'
  shrink = \case
    PeerConnected p -> PeerConnected <$> shrink p
    PeerDisconnected p -> PeerDisconnected <$> shrink p
    HeadIsInitializing headId xs -> HeadIsInitializing <$> shrink headId <*> shrink xs
    Committed headId p u -> Committed <$> shrink headId <*> shrink p <*> shrink u
    HeadIsOpen headId u -> HeadIsOpen <$> shrink headId <*> shrink u
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
    Greetings me headStatus snapshotUtxo -> Greetings <$> shrink me <*> shrink headStatus <*> shrink snapshotUtxo
    PostTxOnChainFailed p e -> PostTxOnChainFailed <$> shrink p <*> shrink e

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
projectHeadStatus :: HeadStatus -> ServerOutput r tx -> HeadStatus
projectHeadStatus headStatus = \case
  HeadIsInitializing{} -> Initializing
  HeadIsOpen{} -> Open
  HeadIsClosed{} -> Closed
  ReadyToFanout{} -> FanoutPossible
  HeadIsFinalized{} -> Final
  _other -> headStatus

-- | Projection function related to 'snapshotUtxo' field in 'Greetings' message.
projectSnapshotUtxo :: Maybe (UTxOType tx) -> ServerOutput r tx -> Maybe (UTxOType tx)
projectSnapshotUtxo snapshotUtxo = \case
  SnapshotConfirmed _ snapshot _ -> Just $ Hydra.Snapshot.utxo snapshot
  HeadIsOpen _ utxos -> Just utxos
  _other -> snapshotUtxo
