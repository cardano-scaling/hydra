{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hydra.API.Server (
  withAPIServer,
  APIServerLog,
) where

import Hydra.Prelude hiding (TVar, readTVar)

import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (newBroadcastTChanIO, writeTChan)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Base16 (encodeBase16)
import Hydra.HeadLogic (
  ClientInput,
  ServerOutput (..),
 )
import Hydra.Ledger (Tx (..))
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (IP, PortNumber)
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
  | APIOutputSent {sentOutput :: LByteString}
  | APIInputReceived {receivedInput :: LByteString}
  | APIInvalidInput {receivedInput :: LByteString}
  deriving (Eq, Show, Generic)

instance ToJSON APIServerLog where
  toJSON = \case
    APIServerStarted{listeningPort} ->
      tagged "APIServerStarted" ["listeningPort" .= toInteger listeningPort]
    NewAPIConnection ->
      tagged "NewAPIConnection" []
    APIOutputSent{sentOutput} ->
      tagged "APIOutputSent" ["sentOutput" .= encodeLByteString sentOutput]
    APIInputReceived{receivedInput} ->
      tagged "APIInputReceived" ["receivedInput" .= encodeLByteString receivedInput]
    APIInvalidInput{receivedInput} ->
      tagged "APIInvalidInput" ["receivedInput" .= encodeLByteString receivedInput]
   where
    tagged :: Text -> [Aeson.Pair] -> Aeson.Value
    tagged tag pairs = object (("tag" .= tag) : pairs)

    encodeLByteString :: LByteString -> Aeson.Value
    encodeLByteString (toStrict -> bytes) =
      case decodeUtf8' bytes of
        Left{} -> toJSON (encodeBase16 bytes)
        Right txt -> toJSON txt

withAPIServer ::
  Tx tx =>
  IP ->
  PortNumber ->
  Tracer IO APIServerLog ->
  (ClientInput tx -> IO ()) ->
  ((ServerOutput tx -> IO ()) -> IO ()) ->
  IO ()
withAPIServer host port tracer inputHandler continuation = do
  responseChannel <- newBroadcastTChanIO
  history <- newTVarIO []
  let sendOutput output = atomically $ do
        modifyTVar' history (output :)
        writeTChan responseChannel output
  race_
    (runAPIServer host port tracer history inputHandler responseChannel)
    (continuation sendOutput)

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
runAPIServer host port tracer history inputHandler responseChannel = do
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
    traceWith tracer (APIOutputSent sentResponse)

  receiveInputs con = forever $ do
    msg <- receiveData con
    case Aeson.eitherDecode msg of
      Right input -> do
        traceWith tracer (APIInputReceived msg)
        inputHandler input
      Left{} -> do
        sendTextData con $ Aeson.encode $ InvalidInput @tx
        traceWith tracer (APIInvalidInput msg)

  forwardHistory con = do
    hist <- STM.atomically (readTVar history)
    let encodeAndReverse xs serverOutput = Aeson.encode serverOutput : xs
    sendTextDatas con $ foldl' encodeAndReverse [] hist
