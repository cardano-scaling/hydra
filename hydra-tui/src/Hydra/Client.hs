{-# LANGUAGE UndecidableInstances #-}

module Hydra.Client where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Control.Concurrent.Async (link)
import Control.Concurrent.Class.MonadSTM (newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Exception (Handler (Handler), IOException, catches)
import Data.Aeson (eitherDecodeStrict, encode)
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.HTTPServer (DraftCommitTxRequest (..), DraftCommitTxResponse (..))
import Hydra.API.ServerOutput (TimedServerOutput)
import Hydra.Cardano.Api (TxId)
import Hydra.Cardano.Api.Prelude (
  AsType (AsPaymentKey, AsSigningKey),
  PaymentKey,
  SigningKey,
 )
import Hydra.Cardano.Api.Tx (signTx)
import Hydra.Chain.CardanoClient (submitTransaction)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Network (Host (Host, hostname, port))
import Hydra.TUI.Options (Options (..))
import Network.HTTP.Req (defaultHttpConfig, responseBody, runReq)
import Network.HTTP.Req qualified as Req
import Network.WebSockets (ConnectionException, receiveData, runClient, sendBinaryData)

data HydraEvent tx
  = ClientConnected
  | ClientDisconnected
  | Update (TimedServerOutput tx)
  | Tick UTCTime
  deriving stock (Generic)

deriving stock instance Eq (TimedServerOutput tx) => Eq (HydraEvent tx)
deriving stock instance Show (TimedServerOutput tx) => Show (HydraEvent tx)

-- | Handle to interact with Hydra node
data Client tx m = Client
  { sendInput :: ClientInput tx -> m ()
  -- ^ Send some input to the server.
  , sk :: SigningKey PaymentKey
  , externalCommit :: UTxO.UTxO -> m ()
  , recoverCommit :: TxId -> m ()
  }

-- | Callback for receiving server outputs.
type ClientCallback tx m = HydraEvent tx -> m ()

-- | A type tying both receiving output and sending input into a /Component/.
type ClientComponent tx m a = ClientCallback tx m -> (Client tx m -> m a) -> m a

-- | Provide a component to interact with Hydra node.
withClient ::
  (ToJSON (ClientInput tx), FromJSON (TimedServerOutput tx)) =>
  Options ->
  ClientComponent tx IO a
withClient Options{hydraNodeHost = Host{hostname, port}, cardanoSigningKey, cardanoNetworkId, cardanoNodeSocket} callback action = do
  sk <- readExternalSk
  q <- newTBQueueIO 10
  withAsync (reconnect $ client q) $ \thread -> do
    -- NOTE(SN): if message formats are not compatible, this will terminate the TUI
    -- with a quite cryptic message (to users)
    link thread -- Make sure it does not silently die
    action $
      Client
        { sendInput = atomically . writeTBQueue q
        , sk
        , externalCommit = externalCommit' sk
        , recoverCommit = recoverCommit'
        }
 where
  readExternalSk = readFileTextEnvelopeThrow (AsSigningKey AsPaymentKey) cardanoSigningKey
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

  externalCommit' sk payload =
    runReq defaultHttpConfig request
      <&> responseBody
        >>= \DraftCommitTxResponse{commitTx} ->
          submitTransaction cardanoNetworkId cardanoNodeSocket $ signTx sk commitTx
   where
    request =
      Req.req
        Req.POST
        (Req.http hostname Req./: "commit")
        (Req.ReqBodyJson $ SimpleCommitRequest @Tx payload)
        Req.jsonResponse
        (Req.port $ fromIntegral port)

  recoverCommit' txId =
    void . runReq defaultHttpConfig $
      Req.req
        Req.DELETE
        (Req.http hostname Req./: "commits" Req./: show txId)
        Req.NoReqBody
        Req.ignoreResponse
        (Req.port $ fromIntegral port)

data ClientError = ClientJSONDecodeError String ByteString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)
