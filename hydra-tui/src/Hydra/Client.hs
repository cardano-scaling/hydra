{-# LANGUAGE UndecidableInstances #-}

module Hydra.Client where

import Hydra.Prelude

import Control.Concurrent.Async (link)
import Control.Concurrent.Class.MonadSTM (readTBQueue, writeTBQueue)
import Control.Exception (Handler (Handler), IOException, catches)
import Data.Aeson (eitherDecodeStrict, encode)
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.HTTPServer (DraftCommitTxRequest (..), DraftCommitTxResponse (..))
import Hydra.API.ServerOutput (ClientMessage, Greetings, InvalidInput, TimedServerOutput)
import Hydra.Cardano.Api (TxId, UTxO)
import Hydra.Cardano.Api.Prelude (
  PaymentKey,
  SigningKey,
 )
import Hydra.Cardano.Api.Tx (signTx)
import Hydra.Chain.Blockfrost.Client qualified as BF
import Hydra.Chain.CardanoClient (submitTransaction)
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Network (Host (Host, hostname, port))
import Hydra.Node.Util (readFileTextEnvelopeThrow)
import Hydra.TUI.Options (Options (..))
import Network.HTTP.Req (defaultHttpConfig, responseBody, runReq)
import Network.HTTP.Req qualified as Req
import Network.WebSockets (Connection, ConnectionException, receiveData, runClient, sendBinaryData)

data HydraEvent tx
  = ClientConnected
  | ClientDisconnected
  | Update (AllPossibleAPIMessages tx)
  | Tick UTCTime
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (HydraEvent tx)
deriving stock instance IsChainState tx => Show (HydraEvent tx)

-- | All possible messages that expect to receive from the hydra-node.
data AllPossibleAPIMessages tx
  = ApiTimedServerOutput (TimedServerOutput tx)
  | ApiClientMessage (ClientMessage tx)
  | ApiGreetings (Greetings tx)
  | ApiInvalidInput InvalidInput
  deriving (Eq, Show)

instance IsChainState tx => FromJSON (AllPossibleAPIMessages tx) where
  parseJSON v =
    (ApiTimedServerOutput <$> parseJSON v)
      <|> (ApiClientMessage <$> parseJSON v)
      <|> (ApiGreetings <$> parseJSON v)
      <|> (ApiInvalidInput <$> parseJSON v)

-- | Handle to interact with Hydra node
data Client tx m = Client
  { sendInput :: ClientInput tx -> m ()
  -- ^ Send some input to the server.
  , sk :: SigningKey PaymentKey
  , externalCommit :: UTxO -> m ()
  , recoverCommit :: TxId -> m ()
  }

-- | Callback for receiving server outputs.
type ClientCallback tx m = HydraEvent tx -> m ()

-- | A type tying both receiving output and sending input into a /Component/.
type ClientComponent tx m a = ClientCallback tx m -> (Client tx m -> m a) -> m a

-- | Provide a component to interact with Hydra node.
withClient ::
  forall tx a.
  IsChainState tx =>
  Options ->
  ClientComponent tx IO a
withClient Options{hydraNodeHost = Host{hostname, port}, cardanoSigningKey, cardanoNetworkId, cardanoConnection} callback action = do
  sk <- readExternalSk
  q <- newLabelledTBQueueIO "tui-client-queue" 10
  withAsyncLabelled ("client-reconnect", reconnect $ client q) $ \thread -> do
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
  readExternalSk = readFileTextEnvelopeThrow cardanoSigningKey
  -- TODO(SN): ping thread?
  client q = runClient (toString hostname) (fromIntegral port) "/?history=yes" $ \con -> do
    -- REVIEW(SN): is sharing the 'con' fine?
    callback ClientConnected
    raceLabelled_ ("receive-outputs", receiveOutputs con) ("send-inputs", sendInputs q con)

  receiveOutputs con = forever $ do
    msg <- receiveData con
    case eitherDecodeStrict msg :: Either String (AllPossibleAPIMessages tx) of
      Left err -> throwIO $ ClientJSONDecodeError err msg
      Right output -> callback $ Update output

  sendInputs :: TBQueue IO (ClientInput tx) -> Connection -> IO ()
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
          case cardanoConnection of
            Left bfProject -> do
              prj <- liftIO $ BF.projectFromFile bfProject
              void $ BF.runBlockfrostM prj $ BF.submitTransaction $ signTx sk commitTx
            Right socketPath ->
              submitTransaction cardanoNetworkId socketPath $ signTx sk commitTx
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
