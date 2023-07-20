{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.Server where

import Hydra.Prelude hiding (TVar, readTVar, seq)

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (newBroadcastTChanIO, writeTChan)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar)
import Control.Exception (IOException)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (pack)
import Data.Version (showVersion)
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.Projection (Projection (..), mkProjection)
import Hydra.API.RestServer (
  DraftCommitTxRequest (..),
  DraftCommitTxResponse (..),
  fromTxOutWithWitness,
 )
import Hydra.API.ServerOutput (
  HeadStatus (Idle),
  OutputFormat (..),
  ServerOutput (Greetings, InvalidInput, hydraNodeVersion),
  ServerOutputConfig (..),
  TimedServerOutput (..),
  WithUTxO (..),
  headStatus,
  me,
  prepareServerOutput,
  projectHeadStatus,
  projectSnapshotUtxo,
  snapshotUtxo,
 )
import Hydra.Chain (
  Chain (..),
  IsChainState,
  PostTxError (
    CannotCommitReferenceScript,
    CommittedTooMuchADAForMainnet,
    SpendingNodeUtxoForbidden,
    UnsupportedLegacyOutput
  ),
 )
import Hydra.Chain.Direct.State ()
import Hydra.Ledger (UTxOType)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (IP, PortNumber)
import qualified Hydra.Options as Options
import Hydra.Party (Party)
import Hydra.Persistence (PersistenceIncremental (..))
import Network.HTTP.Types (Method, status200, status400, status500)
import Network.Wai (
  Request (pathInfo),
  Response,
  ResponseReceived,
  consumeRequestBodyStrict,
  requestMethod,
  responseLBS,
 )
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setBeforeMainLoop,
  setHost,
  setOnException,
  setPort,
 )
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (
  PendingConnection (pendingRequest),
  RequestHead (..),
  acceptRequest,
  defaultConnectionOptions,
  receiveData,
  sendTextData,
  sendTextDatas,
  withPingThread,
 )
import Test.QuickCheck (oneof)
import Text.URI hiding (ParseException)
import Text.URI.QQ (queryKey, queryValue)
import Hydra.Cardano.Api (ProtocolParameters)

data APIServerLog
  = APIServerStarted {listeningPort :: PortNumber}
  | NewAPIConnection
  | APIOutputSent {sentOutput :: Aeson.Value}
  | APIInputReceived {receivedInput :: Aeson.Value}
  | APIInvalidInput {reason :: String, inputReceived :: Text}
  | APIConnectionError {reason :: String}
  | APIHandshakeError {reason :: String}
  | APIRestInputReceived
      { method :: Text
      , paths :: [Text]
      , requestInputBody :: Maybe Aeson.Value
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary APIServerLog where
  arbitrary =
    oneof
      [ APIServerStarted <$> arbitrary
      , pure NewAPIConnection
      , pure $ APIOutputSent (Aeson.Object mempty)
      , pure $ APIInputReceived (Aeson.Object mempty)
      , APIInvalidInput <$> arbitrary <*> arbitrary
      , APIConnectionError <$> arbitrary
      , APIHandshakeError <$> arbitrary
      , APIRestInputReceived
          <$> arbitrary
          <*> arbitrary
          <*> oneof [pure Nothing, pure $ Just (Aeson.Object mempty)]
      ]

-- | Handle to provide a means for sending server outputs to clients.
newtype Server tx m = Server
  { sendOutput :: ServerOutput tx -> m ()
  -- ^ Send some output to all connected clients.
  }

-- | Callback for receiving client inputs.
type ServerCallback tx m = ClientInput tx -> m ()

-- | A type tying both receiving input and sending output into a /Component/.
type ServerComponent tx m a = ServerCallback tx m -> (Server tx m -> m a) -> m a

withAPIServer ::
  forall tx.
  (IsChainState tx) =>
  IP ->
  PortNumber ->
  Party ->
  PersistenceIncremental (TimedServerOutput tx) IO ->
  Tracer IO APIServerLog ->
  Chain tx IO ->
  ProtocolParameters ->
  ServerComponent tx IO ()
withAPIServer host port party PersistenceIncremental{loadAll, append} tracer chain pparams callback action = do
  responseChannel <- newBroadcastTChanIO
  timedOutputEvents <- loadAll

  -- Intialize our read model from stored events
  headStatusP <- mkProjection Idle (output <$> timedOutputEvents) projectHeadStatus
  snapshotUtxoP <- mkProjection Nothing (output <$> timedOutputEvents) projectSnapshotUtxo

  -- NOTE: we need to reverse the list because we store history in a reversed
  -- list in memory but in order on disk
  history <- newTVarIO (reverse timedOutputEvents)
  (notifyServerRunning, waitForServerRunning) <- setupServerNotification
  race_
    (runAPIServer host port party tracer history chain callback headStatusP snapshotUtxoP responseChannel notifyServerRunning pparams)
    ( do
        waitForServerRunning
        action $
          Server
            { sendOutput = \output -> do
                timedOutput <- appendToHistory history output
                atomically $ do
                  update headStatusP output
                  update snapshotUtxoP output
                  writeTChan responseChannel timedOutput
            }
    )
 where
  appendToHistory history output = do
    time <- getCurrentTime
    timedOutput <- atomically $ do
      seq <- nextSequenceNumber history
      let timedOutput = TimedServerOutput{output, time, seq}
      modifyTVar' history (timedOutput :)
      pure timedOutput
    append timedOutput
    pure timedOutput

nextSequenceNumber :: TVar [TimedServerOutput tx] -> STM.STM Natural
nextSequenceNumber historyList =
  STM.readTVar historyList >>= \case
    [] -> pure 0
    (TimedServerOutput{seq} : _) -> pure (seq + 1)

type NotifyServerRunning = IO ()

type WaitForServer = IO ()

-- | Setup notification and waiter to ensure that something only runs after the
-- server is actually listening.
setupServerNotification :: IO (NotifyServerRunning, WaitForServer)
setupServerNotification = do
  mv <- newEmptyMVar
  pure (putMVar mv (), takeMVar mv)

runAPIServer ::
  forall tx.
  (IsChainState tx) =>
  IP ->
  PortNumber ->
  Party ->
  Tracer IO APIServerLog ->
  TVar [TimedServerOutput tx] ->
  Chain tx IO ->
  (ClientInput tx -> IO ()) ->
  -- | Read model to enhance 'Greetings' messages with 'HeadStatus'.
  Projection STM.STM (ServerOutput tx) HeadStatus ->
  -- | Read model to enhance 'Greetings' messages with snapshot UTxO.
  Projection STM.STM (ServerOutput tx) (Maybe (UTxOType tx)) ->
  TChan (TimedServerOutput tx) ->
  -- | Called when the server is listening before entering the main loop.
  NotifyServerRunning ->
  ProtocolParameters ->
  IO ()
runAPIServer host port party tracer history chain callback headStatusP snapshotUtxoP responseChannel notifyServerRunning protocolparams = do
  traceWith tracer (APIServerStarted port)
  -- catch and rethrow with more context
  handle onIOException $
    runSettings serverSettings $
      websocketsOr defaultConnectionOptions wsApp (httpApp chain protocolparams)
 where
  serverSettings =
    defaultSettings
      & setHost (fromString $ show host)
      & setPort (fromIntegral port)
      & setOnException (\_ e -> traceWith tracer $ APIConnectionError{reason = show e})
      & setBeforeMainLoop notifyServerRunning

  wsApp pending = do
    -- XXX: Moving this here improved on flakyness of tests (which would see a
    -- 'HandshakeException'). This indicates that there might be a race
    -- condition between notifyingServerRunning and clients starting and
    -- handshaking on connections still?
    traceWith tracer NewAPIConnection
    let path = requestPath $ pendingRequest pending
    queryParams <- uriQuery <$> mkURIBs path
    con <- acceptRequest pending
    chan <- STM.atomically $ dupTChan responseChannel

    -- api client can decide if they want to see the past history of server outputs
    unless (shouldNotServeHistory queryParams) $
      forwardHistory con

    forwardGreetingOnly con

    let outConfig = mkServerOutputConfig queryParams

    withPingThread con 30 (pure ()) $
      race_ (receiveInputs con) (sendOutputs chan con outConfig)

  -- Hydra HTTP server
  httpApp directChain pparams' req respond =
    case (requestMethod req, pathInfo req) of
      ("POST", ["commit"]) -> do
        body <- consumeRequestBodyStrict req
        handleDraftCommitUtxo directChain tracer body (requestMethod req) (pathInfo req) respond
      ("GET", ["protocol-parameters"]) ->
        respond $ responseLBS status200 [] (Aeson.encode pparams')
      _ -> do
        traceWith tracer $
          APIRestInputReceived
            { method = decodeUtf8 $ requestMethod req
            , paths = pathInfo req
            , requestInputBody = Nothing
            }
        respond $ responseLBS status400 [] "Resource not found"

  -- NOTE: We will add a 'Greetings' message on each API server start. This is
  -- important to make sure the latest configured 'party' is reaching the
  -- client.
  forwardGreetingOnly con = do
    seq <- atomically $ nextSequenceNumber history
    headStatus <- atomically getLatestHeadStatus
    snapshotUtxo <- atomically getLatestSnapshotUtxo
    time <- getCurrentTime

    sendTextData con $
      Aeson.encode
        TimedServerOutput
          { time
          , seq
          , output =
              Greetings
                { me = party
                , headStatus
                , snapshotUtxo
                , hydraNodeVersion = showVersion Options.hydraNodeVersion
                } ::
                ServerOutput tx
          }

  Projection{getLatest = getLatestHeadStatus} = headStatusP
  Projection{getLatest = getLatestSnapshotUtxo} = snapshotUtxoP

  mkServerOutputConfig qp =
    ServerOutputConfig
      { txOutputFormat = decideOnTxDisplay qp
      , utxoInSnapshot = decideOnUTxODisplay qp
      }

  decideOnTxDisplay qp =
    let k = [queryKey|tx-output|]
        v = [queryValue|cbor|]
        queryP = QueryParam k v
     in if queryP `elem` qp then OutputCBOR else OutputJSON

  decideOnUTxODisplay qp =
    let k = [queryKey|snapshot-utxo|]
        v = [queryValue|no|]
        queryP = QueryParam k v
     in if queryP `elem` qp then WithoutUTxO else WithUTxO

  shouldNotServeHistory qp =
    flip any qp $ \case
      (QueryParam key val)
        | key == [queryKey|history|] -> val == [queryValue|no|]
      _other -> False

  onIOException ioException =
    throwIO $
      RunServerException
        { ioException
        , host
        , port
        }

  sendOutputs chan con outConfig = forever $ do
    response <- STM.atomically $ readTChan chan
    let sentResponse =
          prepareServerOutput outConfig response

    sendTextData con sentResponse
    traceWith tracer (APIOutputSent $ toJSON response)

  receiveInputs con = forever $ do
    msg <- receiveData con
    case Aeson.eitherDecode msg of
      Right input -> do
        traceWith tracer (APIInputReceived $ toJSON input)
        callback input
      Left e -> do
        -- XXX(AB): toStrict might be problematic as it implies consuming the full
        -- message to memory
        let clientInput = decodeUtf8With lenientDecode $ toStrict msg
        time <- getCurrentTime
        seq <- atomically $ nextSequenceNumber history
        let timedOutput = TimedServerOutput{output = InvalidInput @tx e clientInput, time, seq}
        sendTextData con $ Aeson.encode timedOutput
        traceWith tracer (APIInvalidInput e clientInput)

  forwardHistory con = do
    hist <- STM.atomically (readTVar history)
    let encodeAndReverse xs serverOutput = Aeson.encode serverOutput : xs
    sendTextDatas con $ foldl' encodeAndReverse [] hist

data RunServerException = RunServerException
  { ioException :: IOException
  , host :: IP
  , port :: PortNumber
  }
  deriving (Show)

instance Exception RunServerException

-- Handle user requests to obtain a draft commit tx
handleDraftCommitUtxo ::
  Chain tx IO ->
  Tracer IO APIServerLog ->
  LBS.ByteString ->
  Method ->
  [Text] ->
  (Response -> IO ResponseReceived) ->
  IO ResponseReceived
handleDraftCommitUtxo directChain tracer body reqMethod reqPaths respond = do
  case Aeson.eitherDecode' body :: Either String DraftCommitTxRequest of
    Left err ->
      respond $ responseLBS status400 [] (Aeson.encode $ Aeson.String $ pack err)
    Right requestInput@DraftCommitTxRequest{utxoToCommit} -> do
      traceWith tracer $
        APIRestInputReceived
          { method = decodeUtf8 reqMethod
          , paths = reqPaths
          , requestInputBody = Just $ toJSON requestInput
          }
      eCommitTx <- draftCommitTx $ fromTxOutWithWitness <$> utxoToCommit
      respond $
        case eCommitTx of
          Left e ->
            -- Distinguish between errors users can actually benefit from and
            -- other errors that are turned into 500 responses.
            case e of
              CannotCommitReferenceScript -> return400 e
              CommittedTooMuchADAForMainnet _ _ -> return400 e
              UnsupportedLegacyOutput _ -> return400 e
              walletUtxoErr@SpendingNodeUtxoForbidden -> return400 walletUtxoErr
              _ -> responseLBS status500 [] (Aeson.encode $ toJSON e)
          Right commitTx ->
            responseLBS status200 [] (Aeson.encode $ DraftCommitTxResponse commitTx)
 where
  return400 = responseLBS status400 [] . Aeson.encode . toJSON

  Chain{draftCommitTx} = directChain
