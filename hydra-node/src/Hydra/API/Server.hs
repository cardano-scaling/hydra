{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.Server (
  Server (..),
  ServerCallback,
  ServerComponent,
  withAPIServer,
  APIServerLog,
) where

import Hydra.Prelude hiding (TVar, readTVar)

import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (newBroadcastTChanIO, writeTChan)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar)
import Control.Exception (IOException)
import qualified Data.Aeson as Aeson
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.ServerOutput (ServerOutput (Greetings, InvalidInput), TimedServerOutput (..))
import Hydra.Chain (IsChainState)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (IP, PortNumber)
import Hydra.Party (Party)
import Hydra.Persistence (PersistenceIncremental (..))
import Network.WebSockets (
  acceptRequest,
  receiveData,
  runServer,
  sendTextData,
  sendTextDatas,
  withPingThread,
 )
import Test.QuickCheck (oneof)

data APIServerLog
  = APIServerStarted {listeningPort :: PortNumber}
  | NewAPIConnection
  | APIOutputSent {sentOutput :: Aeson.Value}
  | APIInputReceived {receivedInput :: Aeson.Value}
  | APIInvalidInput {reason :: String, inputReceived :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance Arbitrary APIServerLog where
  arbitrary =
    oneof
      [ APIServerStarted <$> arbitrary
      , pure NewAPIConnection
      , pure $ APIOutputSent (Aeson.Object mempty)
      , pure $ APIInputReceived (Aeson.Object mempty)
      , APIInvalidInput <$> arbitrary <*> arbitrary
      ]

-- | Handle to provide a means for sending server outputs to clients.
newtype Server tx m = Server
  { -- | Send some output to all connected clients.
    sendOutput :: ServerOutput tx -> m ()
  }

-- | Callback for receiving client inputs.
type ServerCallback tx m = ClientInput tx -> m ()

-- | A type tying both receiving input and sending output into a /Component/.
type ServerComponent tx m a = ServerCallback tx m -> (Server tx m -> m a) -> m a

withAPIServer ::
  (IsChainState tx) =>
  IP ->
  PortNumber ->
  Party ->
  PersistenceIncremental (TimedServerOutput tx) IO ->
  Tracer IO APIServerLog ->
  ServerComponent tx IO ()
withAPIServer host port party PersistenceIncremental{loadAll, append} tracer callback action = do
  responseChannel <- newBroadcastTChanIO
  h <- loadAll
  -- NOTE: We will add a 'Greetings' message on each API server start. This is
  -- important to make sure the latest configured 'party' is reaching the
  -- client.
  -- NOTE: we need to reverse the list because we store history in a reversed
  -- list in memory but in order on disk
  history <- newTVarIO (reverse h)

  now <- getCurrentTime
  let greetings = TimedServerOutput{output = Greetings party, time = now}
  appendToHistory history greetings
  race_
    (runAPIServer host port tracer history callback responseChannel)
    . action
    $ Server
      { sendOutput = \output -> do
          now' <- getCurrentTime
          let timedOutput = TimedServerOutput{output, time = now'}
          appendToHistory history timedOutput
          atomically $
            writeTChan responseChannel timedOutput
      }
 where
  appendToHistory history event = do
    append event
    atomically $
      modifyTVar' history (event :)

runAPIServer ::
  forall tx.
  (IsChainState tx) =>
  IP ->
  PortNumber ->
  Tracer IO APIServerLog ->
  TVar [TimedServerOutput tx] ->
  (ClientInput tx -> IO ()) ->
  TChan (TimedServerOutput tx) ->
  IO ()
runAPIServer host port tracer history callback responseChannel = do
  traceWith tracer (APIServerStarted port)
  handle onIOException $
    runServer (show host) (fromIntegral port) $ \pending -> do
      con <- acceptRequest pending
      chan <- STM.atomically $ dupTChan responseChannel
      traceWith tracer NewAPIConnection
      forwardHistory con
      withPingThread con 30 (pure ()) $
        race_ (receiveInputs con) (sendOutputs chan con)
 where
  onIOException ioException =
    throwIO $
      RunServerException
        { ioException
        , host
        , port
        }

  sendOutputs chan con = forever $ do
    response <- STM.atomically $ readTChan chan
    let sentResponse = Aeson.encode response
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
        now' <- getCurrentTime
        let timedOutput = TimedServerOutput{output = InvalidInput @tx e clientInput, time = now'}
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
