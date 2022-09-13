module Hydra.Client where

import Hydra.Prelude

import Control.Concurrent.Async (link)
import Control.Exception (Handler (Handler), IOException, catches)
import Control.Monad.Class.MonadSTM (newTBQueueIO, readTBQueue, writeTBQueue)
import Data.Aeson (eitherDecodeStrict, encode)
import Hydra.Cardano.Api (
  AsType (AsPaymentKey, AsSigningKey),
  PaymentKey,
  SigningKey,
 )
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.ClientInput (ClientInput)
import Hydra.Ledger (IsTx)
import Hydra.Network (Host (Host, hostname, port))
import Hydra.ServerOutput (ServerOutput)
import Hydra.TUI.Options (Options (..))
import Network.WebSockets (ConnectionException, receiveData, runClient, sendBinaryData)

data HydraEvent tx
  = ClientConnected
  | ClientDisconnected
  | Update (ServerOutput tx)
  | Tick UTCTime
  deriving (Eq, Show, Generic)

-- | Handle to interact with Hydra node
data Client tx m = Client
  { -- | Send some input to the server.
    sendInput :: ClientInput tx -> m ()
  , sk :: SigningKey PaymentKey
  }

-- | Callback for receiving server outputs.
type ClientCallback tx m = HydraEvent tx -> m ()

-- | A type tying both receiving output and sending input into a /Component/.
type ClientComponent tx m a = ClientCallback tx m -> (Client tx m -> m a) -> m a

-- | Provide a component to interact with Hydra node.
withClient :: IsTx tx => Options -> ClientComponent tx IO a
withClient Options{hydraNodeHost = Host{hostname, port}, cardanoSigningKey} callback action = do
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsPaymentKey) cardanoSigningKey
  q <- newTBQueueIO 10
  withAsync (reconnect $ client q) $ \thread -> do
    -- NOTE(SN): if message formats are not compatible, this will terminate the TUI
    -- with a quite cryptic message (to users)
    link thread -- Make sure it does not silently die
    action $
      Client
        { sendInput = atomically . writeTBQueue q
        , sk
        }
 where
  -- TODO(SN): ping thread?
  client q = runClient (toString hostname) (fromIntegral port) "/" $ \con -> do
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
