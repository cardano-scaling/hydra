module Hydra.Client where

import Hydra.Prelude

import Hydra.ClientInput (ClientInput)
import Hydra.Ledger (Tx)
import Hydra.ServerOutput (ServerOutput)
import Network.WebSockets (receiveData, runClient)
import Data.Aeson (eitherDecodeStrict)

-- | Handle to provide a means for sending inputs to the server.
newtype Client tx m = Client
  { -- | Send some input to the server.
    sendInput :: ClientInput tx -> m ()
  }

-- | Callback for receiving server outputs.
type ClientCallback tx m = ServerOutput tx -> m ()

-- | A type tying both receiving output and sending input into a /Component/.
type ClientComponent tx m a = ClientCallback tx m -> (Client tx m -> m a) -> m a

-- | Connect to a hydra-node using a websocket and provide a /Component/ for
-- sending client inputs, as well as receiving server outputs.
withClient :: Tx tx => ClientComponent tx IO a
withClient callback action =
  withAsync client $ \_ ->
    action $
      Client
        { sendInput = \input ->
            putText $ "should send: " <> show input
        }
 where
  -- TODO(SN): parameterize
  -- TODO(SN): re-establish connection on error
  -- TODO(SN): ping thread?
  client = runClient "127.0.0.1" 4001 "/" $ \con ->
    forever $ do
      msg <- receiveData con
      case eitherDecodeStrict msg of
        Right output -> callback output
        Left err -> throwIO $ ClientJSONDecodeError err

newtype ClientError = ClientJSONDecodeError String
  deriving (Eq, Show, Generic)
  deriving anyclass Exception
