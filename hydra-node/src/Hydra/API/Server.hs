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
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Hydra.ClientInput (ClientInput)
import Hydra.Ledger (Tx (..))
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (IP, PortNumber)
import Hydra.Party (Party)
import Hydra.ServerOutput (ServerOutput (Greetings, InvalidInput))
import Network.WebSockets (
  acceptRequest,
  receiveData,
  runServer,
  sendTextData,
  sendTextDatas,
  withPingThread,
 )

data APIServerLog
  = APIServerStarted {listeningPort :: PortNumber}
  | NewAPIConnection
  | APIOutputSent {sentOutput :: Aeson.Value}
  | APIInputReceived {receivedInput :: Aeson.Value}
  | APIInvalidInput {reason :: String, inputReceived :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance Arbitrary Value => Arbitrary APIServerLog where
  arbitrary = genericArbitrary

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
  Tx tx =>
  IP ->
  PortNumber ->
  Party ->
  Tracer IO APIServerLog ->
  ServerComponent tx IO ()
withAPIServer host port party tracer callback action = do
  responseChannel <- newBroadcastTChanIO
  history <- newTVarIO [Greetings party]
  race_
    (runAPIServer host port tracer history callback responseChannel)
    . action
    $ Server
      { sendOutput = \output -> atomically $ do
          modifyTVar' history (output :)
          writeTChan responseChannel output
      }

runAPIServer ::
  forall tx.
  Tx tx =>
  IP ->
  PortNumber ->
  Tracer IO APIServerLog ->
  TVar [ServerOutput tx] ->
  (ClientInput tx -> IO ()) ->
  TChan (ServerOutput tx) ->
  IO ()
runAPIServer host port tracer history callback responseChannel = do
  traceWith tracer (APIServerStarted port)
  runServer (show host) (fromIntegral port) $ \pending -> do
    con <- acceptRequest pending
    chan <- STM.atomically $ dupTChan responseChannel
    traceWith tracer NewAPIConnection
    forwardHistory con
    withPingThread con 30 (pure ()) $
      race_ (receiveInputs con) (sendOutputs chan con)
 where
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
        sendTextData con $ Aeson.encode $ InvalidInput @tx e clientInput
        traceWith tracer (APIInvalidInput e clientInput)

  forwardHistory con = do
    hist <- STM.atomically (readTVar history)
    let encodeAndReverse xs serverOutput = Aeson.encode serverOutput : xs
    sendTextDatas con $ foldl' encodeAndReverse [] hist
