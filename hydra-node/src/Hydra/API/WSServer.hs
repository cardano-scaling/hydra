{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.WSServer where

import Hydra.Prelude hiding (TVar, filter, readTVar, seq)

import Cardano.Binary (serialize')
import Conduit (ConduitT, ResourceT, mapM_C, runConduitRes, (.|))
import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import Control.Concurrent.STM qualified as STM
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Conduit.Combinators (filter)
import Data.Version (showVersion)
import Hydra.API.APIServerLog (APIServerLog (..))
import Hydra.API.ClientInput (ClientInput (SafeClose))
import Hydra.API.Projection (Projection (..))
import Hydra.API.ServerOutput (
  ApiEncoding (..),
  ClientMessage,
  Greetings (..),
  HeadStatus (..),
  InvalidInput (..),
  NetworkInfo,
  ServerOutputConfig (..),
  TimedServerOutput (..),
  WithAddressedTx (..),
  WithUTxO (..),
  getSnapshotUtxo,
  handleUtxoInclusion,
  handleUtxoInclusionTyped,
  headStatus,
  me,
  prepareServerOutput,
  removeSnapshotUTxO,
  snapshotUtxo,
 )
import Hydra.API.ServerOutputFilter (
  ServerOutputFilter (..),
 )
import Hydra.API.WireFormat (decodeWire, describeWire)
import Hydra.Chain (Chain (..))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic (ClosedState (ClosedState, readyToFanoutSent), HeadState, OpenState (..), StateChanged)
import Hydra.HeadLogic.State qualified as HeadState
import Hydra.Logging (Tracer, traceWith)
import Hydra.NetworkVersions qualified as NetworkVersions
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (ChainPointTime (..), NodeState (..), syncedStatus)
import Hydra.Tx (HeadId, Party)
import Network.WebSockets (
  Connection,
  PendingConnection (pendingRequest),
  RequestHead (..),
  acceptRequest,
  receiveData,
  sendBinaryData,
  sendTextData,
  withPingThread,
 )
import Text.URI hiding (ParseException)
import Text.URI.QQ (queryKey, queryValue)

-- | Per-connection codec: resolves the negotiated wire encoding and the
-- snapshot-utxo display policy once, so message handling needs no dispatch.
data WsCodec tx = WsCodec
  { sendOutput :: TimedServerOutput tx -> IO ()
  , sendClientMessage :: ClientMessage tx -> IO ()
  , sendGreetings :: Greetings tx -> IO ()
  , sendInvalidInput :: InvalidInput -> IO ()
  , decodeInput :: LBS.ByteString -> Either String (ClientInput tx)
  , describeInput :: LBS.ByteString -> Text
  }

-- | Resolve the negotiated encoding into a 'WsCodec'. This is the only place
-- deciding how messages go onto (and come off) the wire: JSON as text frames,
-- CBOR as binary frames.
--
-- NOTE: Inputs are decoded per the negotiated encoding, never the frame type:
-- some JSON clients (e.g. the TUI) send binary frames containing JSON.
mkWsCodec :: IsChainState tx => ServerOutputConfig -> Connection -> WsCodec tx
mkWsCodec config con =
  case config.encoding of
    JsonEncoding ->
      WsCodec
        { sendOutput = sendTextData con . prepareServerOutput config
        , sendClientMessage = sendTextData con . handleUtxoInclusion config removeSnapshotUTxO . Aeson.encode
        , sendGreetings = sendPlainJson
        , sendInvalidInput = sendPlainJson
        , decodeInput = decodeWire JsonEncoding
        , describeInput = describeWire JsonEncoding
        }
    CborEncoding ->
      WsCodec
        { sendOutput = sendBinaryData con . serialize' . handleUtxoInclusionTyped config
        , -- NOTE: The byte-level utxo filtering does not apply to CBOR;
          -- 'ClientMessage' is sent unfiltered.
          sendClientMessage = sendPlainCbor
        , sendGreetings = sendPlainCbor
        , sendInvalidInput = sendPlainCbor
        , decodeInput = decodeWire CborEncoding
        , describeInput = describeWire CborEncoding
        }
 where
  sendPlainJson :: ToJSON a => a -> IO ()
  sendPlainJson = sendTextData con . Aeson.encode

  sendPlainCbor :: ToCBOR a => a -> IO ()
  sendPlainCbor = sendBinaryData con . serialize'

wsApp ::
  forall tx.
  IsChainState tx =>
  Environment ->
  Party ->
  Tracer IO APIServerLog ->
  Chain tx IO ->
  ConduitT () (TimedServerOutput tx) (ResourceT IO) () ->
  (ClientInput tx -> IO ()) ->
  -- | Read model to enhance 'Greetings' messages with 'HeadStatus'.
  Projection STM.STM (StateChanged tx) (NodeState tx) ->
  -- | Read model to enhance 'Greetings' messages with 'NetworkInfo'.
  Projection STM.STM (StateChanged tx) NetworkInfo ->
  TChan (Either (TimedServerOutput tx) (ClientMessage tx)) ->
  ServerOutputFilter tx ->
  PendingConnection ->
  IO ()
wsApp env party tracer chain history callback nodeStateP networkInfoP responseChannel ServerOutputFilter{txContainsAddr} pending = do
  traceWith tracer NewAPIConnection
  let path = requestPath $ pendingRequest pending
  queryParams <- uriQuery <$> mkURIBs path
  con <- acceptRequest pending
  chan <- STM.atomically $ dupTChan responseChannel

  let outConfig = mkServerOutputConfig queryParams
      codec = mkWsCodec outConfig con

  -- api client can decide if they want to see the past history of server outputs
  when (shouldServeHistory queryParams) $
    forwardHistory codec outConfig

  forwardGreetingOnly codec outConfig

  withPingThread con 30 (pure ()) $
    raceLabelled_
      ("ws-con-receive-inputs", receiveInputs codec con)
      ("ws-con-send-outputs", sendOutputs codec chan outConfig)
 where
  -- NOTE: We will add a 'Greetings' message on each API server start. This is
  -- important to make sure the latest configured 'party' is reaching the
  -- client.
  forwardGreetingOnly codec config = do
    nodeState <- atomically getLatestNodeState
    let headState = nodeState.headState
    networkInfo <- atomically getLatestNetworkInfo
    let greetings =
          Greetings
            { me = party
            , headStatus = getHeadStatus headState
            , hydraHeadId = getHeadId headState
            , snapshotUtxo =
                case config.utxoInSnapshot of
                  WithUTxO -> getSnapshotUtxo headState
                  WithoutUTxO -> Nothing
            , hydraNodeVersion = showVersion NetworkVersions.hydraNodeVersion
            , env
            , networkInfo
            , chainSyncedStatus = syncedStatus nodeState
            , currentSlot = nodeState.chainPointTime.currentSlot
            }
    codec.sendGreetings greetings

  Projection{getLatest = getLatestNodeState} = nodeStateP
  Projection{getLatest = getLatestNetworkInfo} = networkInfoP

  mkServerOutputConfig :: [QueryParam] -> ServerOutputConfig
  mkServerOutputConfig qp =
    ServerOutputConfig
      { utxoInSnapshot = decideOnUTxODisplay qp
      , addressInTx = decideOnAddressDisplay qp
      , encoding = decideOnEncoding qp
      }

  decideOnEncoding :: [QueryParam] -> ApiEncoding
  decideOnEncoding qp =
    let queryP = QueryParam [queryKey|encoding|] [queryValue|cbor|]
     in if queryP `elem` qp then CborEncoding else JsonEncoding

  decideOnUTxODisplay :: [QueryParam] -> WithUTxO
  decideOnUTxODisplay qp =
    let k :: RText t
        k = [queryKey|snapshot-utxo|]
        v :: RText t
        v = [queryValue|no|]
        queryP = QueryParam k v
     in if queryP `elem` qp then WithoutUTxO else WithUTxO

  decideOnAddressDisplay :: [QueryParam] -> WithAddressedTx
  decideOnAddressDisplay qp =
    case find queryByAddress qp of
      Just (QueryParam _ v) -> WithAddressedTx (unRText v)
      _ -> WithoutAddressedTx
   where
    queryByAddress = \case
      (QueryParam key _) | key == [queryKey|address|] -> True
      _other -> False

  shouldServeHistory :: [QueryParam] -> Bool
  shouldServeHistory qp =
    flip any qp $ \case
      (QueryParam key val)
        | key == [queryKey|history|] -> val == [queryValue|yes|]
      _other -> False

  sendOutputs codec chan ServerOutputConfig{addressInTx} = forever $ do
    response <- STM.atomically $ readTChan chan
    when (isAddressInTx addressInTx response) $
      sendResponse response
   where
    sendResponse = \case
      Left response -> do
        codec.sendOutput response
        traceWith tracer (APIOutputSent $ toJSON response)
      Right response -> do
        codec.sendClientMessage response
        traceWith tracer (APIOutputSent $ toJSON response)

  Chain{checkNonADAAssets} = chain

  receiveInputs codec con = forever $ do
    msg <- receiveData con
    let receivedText = codec.describeInput msg
    case codec.decodeInput msg of
      Right input -> do
        traceWith tracer (APIInputReceived $ toJSON input)
        case input of
          SafeClose -> do
            nodeState <- atomically getLatestNodeState
            case HeadState.getOpenStateConfirmedSnapshot nodeState.headState of
              Nothing -> callback input
              Just confirmedSnapshot ->
                case checkNonADAAssets confirmedSnapshot of
                  Left nonADAValue -> do
                    let errorStr = "Cannot SafeClose with non-ADA assets present: " <> show nonADAValue
                    codec.sendInvalidInput $ InvalidInput errorStr receivedText
                    traceWith tracer (APIInvalidInput errorStr receivedText)
                  Right _ -> callback input
          _ -> callback input
      Left e -> do
        -- XXX(AB): toStrict might be problematic as it implies consuming the full
        -- message to memory
        codec.sendInvalidInput $ InvalidInput e receivedText
        traceWith tracer (APIInvalidInput e receivedText)

  forwardHistory codec ServerOutputConfig{addressInTx} =
    runConduitRes $ history .| filter (isAddressInTx addressInTx . Left) .| mapM_C (liftIO . codec.sendOutput)

  isAddressInTx addressInTx = \case
    Left tx -> checkAddress tx
    Right _ -> True
   where
    checkAddress tx =
      case addressInTx of
        WithAddressedTx addr -> txContainsAddr tx addr
        WithoutAddressedTx -> True

  -- \| Get the content of 'headStatus' field in 'Greetings' message from the full 'HeadState'.
  getHeadStatus :: HeadState tx -> HeadStatus
  getHeadStatus = \case
    HeadState.Idle{} -> Idle
    HeadState.Open{} -> Open
    HeadState.Closed ClosedState{readyToFanoutSent}
      | readyToFanoutSent -> FanoutPossible
      | otherwise -> Closed

  getHeadId :: HeadState tx -> Maybe HeadId
  getHeadId = \case
    HeadState.Idle{} -> Nothing
    HeadState.Open OpenState{headId} -> Just headId
    HeadState.Closed ClosedState{headId} -> Just headId
