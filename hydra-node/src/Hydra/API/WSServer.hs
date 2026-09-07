{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.WSServer where

import Hydra.Prelude hiding (TVar, filter, readTVar, seq)

import Cardano.Binary (serialize')
import Conduit (ConduitT, ResourceT, mapM_C, runConduitRes, (.|))
import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import Control.Concurrent.STM qualified as STM
import Control.Tracer.JSON (Tracer, traceWith)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
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
  handleUtxoInclusionTyped,
  headStatus,
  me,
  prepareServerOutput,
  snapshotUtxo,
 )
import Hydra.API.ServerOutputFilter (
  ServerOutputFilter (..),
 )
import Hydra.API.WireFormat (decodeWire, describeWire)
import Hydra.Chain (Chain (..))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic (ClosedState (ClosedState, readyToFanoutSent), HeadState, OpenState (..), PartialFanoutState (..), StateChanged)
import Hydra.HeadLogic.State qualified as HeadState
import Hydra.NetworkVersions qualified as NetworkVersions
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (ChainPointTime (..), NodeState (..), syncedStatus)
import Hydra.Tx (HeadId, Party)
import Network.HTTP.Types.URI (Query, parseQuery)
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
        , -- NOTE: 'ClientMessage' has no top-level snapshot, so the
          -- snapshot-utxo filter does not apply to it.
          sendClientMessage = sendPlainJson
        , sendGreetings = sendPlainJson
        , sendInvalidInput = sendPlainJson
        , decodeInput = decodeWire JsonEncoding
        , describeInput = describeWire JsonEncoding
        }
    CborEncoding ->
      WsCodec
        { sendOutput = sendBinaryData con . serialize' . handleUtxoInclusionTyped config
        , -- NOTE: 'ClientMessage' has no top-level snapshot, so the
          -- snapshot-utxo filter does not apply to it.
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
  let queryParams = queryParamsOf . requestPath $ pendingRequest pending
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
    HeadState.FanoutProgress{} -> FanningOut

  getHeadId :: HeadState tx -> Maybe HeadId
  getHeadId = \case
    HeadState.Idle{} -> Nothing
    HeadState.Open OpenState{headId} -> Just headId
    HeadState.Closed ClosedState{headId} -> Just headId
    HeadState.FanoutProgress PartialFanoutState{headId} -> Just headId

-- | The query parameters of a websocket connection request path.
--
-- NOTE: a malformed query string yields no parameters here, and hence the
-- default output config. modern-uri (used here previously) raised a parse
-- exception during connection setup instead.
queryParamsOf :: ByteString -> Query
queryParamsOf = parseQuery . BS8.dropWhile (/= '?')

-- | Decide what a client wants to see, from the query string of its connection.
mkServerOutputConfig :: Query -> ServerOutputConfig
mkServerOutputConfig qp =
  ServerOutputConfig
    { utxoInSnapshot = decideOnUTxODisplay qp
    , addressInTx = decideOnAddressDisplay qp
    , encoding = decideOnEncoding qp
    }

decideOnEncoding :: Query -> ApiEncoding
decideOnEncoding qp =
  if ("encoding", Just "cbor") `elem` qp then CborEncoding else JsonEncoding

decideOnUTxODisplay :: Query -> WithUTxO
decideOnUTxODisplay qp =
  if ("snapshot-utxo", Just "no") `elem` qp then WithoutUTxO else WithUTxO

decideOnAddressDisplay :: Query -> WithAddressedTx
decideOnAddressDisplay qp =
  -- NOTE: takes the first 'address' that actually carries a value. A valueless
  -- '?address' is skipped rather than disabling the filter: modern-uri (used
  -- here previously) parsed that as a QueryFlag, which the old lookup ignored,
  -- so '?address&address=addr1...' still filtered. An empty '?address=' is
  -- skipped for the same reason: the filter compares addresses exactly, so
  -- keeping it would match nothing and the client would silently see no
  -- transaction outputs at all.
  case listToMaybe [v | ("address", Just v) <- qp, not (BS8.null v)] of
    Just v -> WithAddressedTx (decodeUtf8 v)
    Nothing -> WithoutAddressedTx

shouldServeHistory :: Query -> Bool
shouldServeHistory qp =
  ("history", Just "yes") `elem` qp
