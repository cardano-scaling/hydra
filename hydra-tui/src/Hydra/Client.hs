module Hydra.Client where

import Hydra.Prelude

import Control.Concurrent.Async (link)
import Control.Exception (Handler (Handler), IOException, catches)
import Data.Aeson (eitherDecodeStrict)
import Hydra.ClientInput (ClientInput)
import Hydra.Ledger (Tx)
import Hydra.Network (Host (Host, hostName, portNumber))
import Hydra.ServerOutput (ServerOutput)
import Network.WebSockets (ConnectionException, receiveData, runClient)

data HydraEvent tx
  = ClientConnected
  | ClientDisconnected
  | Update (ServerOutput tx)
  deriving (Eq, Show, Generic)

-- | Handle to provide a means for sending inputs to the server.
newtype Client tx m = Client
  { -- | Send some input to the server.
    sendInput :: ClientInput tx -> m ()
  }

-- | Callback for receiving server outputs.
type ClientCallback tx m = HydraEvent tx -> m ()

-- | A type tying both receiving output and sending input into a /Component/.
type ClientComponent tx m a = ClientCallback tx m -> (Client tx m -> m a) -> m a

-- | Connect to a hydra-node using a websocket and provide a /Component/ for
-- sending client inputs, as well as receiving server outputs.
withClient :: Tx tx => Host -> ClientComponent tx IO a
withClient Host{hostName, portNumber} callback action = do
  withAsync client $ \thread -> do
    link thread -- Make sure it does not silently die
    action $
      Client
        { sendInput = \input ->
            putText $ "should send: " <> show input
        }
 where
  client =
    connect
      `catches` [ Handler $ \(_ :: IOException) -> handleDisconnect -- Initially
                , Handler $ \(_ :: ConnectionException) -> handleDisconnect -- Later
                ]

  handleDisconnect = do
    callback ClientDisconnected
    threadDelay 1
    client

  -- TODO(SN): ping thread?
  connect = runClient (toString hostName) (fromIntegral portNumber) "/" $ \con -> do
    callback ClientConnected
    forever $ do
      msg <- receiveData con
      case eitherDecodeStrict msg of
        Right output -> callback $ Update output
        Left err -> throwIO $ ClientJSONDecodeError err

newtype ClientError = ClientJSONDecodeError String
  deriving (Eq, Show, Generic)
  deriving anyclass (Exception)
