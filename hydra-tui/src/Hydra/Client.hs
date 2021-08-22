module Hydra.Client where

import Hydra.Prelude

import Control.Concurrent.Async (link)
import Control.Exception (Handler (Handler), IOException, catches)
import Control.Monad.Class.MonadSTM (MonadSTM (newTBQueueIO), MonadSTMTx (readTBQueue, writeTBQueue))
import Data.Aeson (eitherDecodeStrict, encode)
import Hydra.ClientInput (ClientInput)
import Hydra.Ledger (Tx)
import Hydra.Network (Host (Host, hostName, portNumber))
import Hydra.ServerOutput (ServerOutput)
import Network.WebSockets (ConnectionException, receiveData, runClient, sendBinaryData)

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
  q <- newTBQueueIO 10
  withAsync (reconnect $ client q) $ \thread -> do
    -- NOTE(SN): message formats are not compatible, this will terminate the TUI
    -- with a quite cryptic message (to users)
    link thread -- Make sure it does not silently die
    action $
      Client
        { sendInput = atomically . writeTBQueue q
        }
 where
  -- TODO(SN): ping thread?
  client q = runClient (toString hostName) (fromIntegral portNumber) "/" $ \con -> do
    -- REVIEW(SN): is sharing the 'con' fine?
    callback ClientConnected
    race_ (receiveOutputs con) (sendInputs q con)

  receiveOutputs con = forever $ do
    msg <- receiveData con
    case eitherDecodeStrict msg of
      Right output -> callback $ Update output
      Left err -> throwIO $ ClientJSONDecodeError err msg

  sendInputs q con = forever $ do
    input <- atomically $ readTBQueue q
    sendBinaryData con $ encode input

  reconnect f =
    f
      `catches` [ Handler $ \(_ :: IOException) -> handleDisconnect f -- Initially
                , Handler $ \(_ :: ConnectionException) -> handleDisconnect f -- Later
                ]

  handleDisconnect f =
    callback ClientDisconnected >> threadDelay 1 >> reconnect f

data ClientError = ClientJSONDecodeError String ByteString
  deriving (Eq, Show, Generic)
  deriving anyclass (Exception)
