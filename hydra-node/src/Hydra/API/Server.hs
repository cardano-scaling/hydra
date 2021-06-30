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
import qualified Data.Aeson as Aeson
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
  | APIResponseSent {sentResponse :: LByteString}
  | APIRequestReceived {receivedRequest :: LByteString}
  | APIInvalidRequest {receivedRequest :: LByteString}
  deriving (Eq, Show)

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
        modifyTVar' history (Right output :)
        writeTChan responseChannel output
  let recvInput input = do
        atomically $ modifyTVar' history (Left input :)
        inputHandler input
  race_
    (runAPIServer host port tracer history recvInput responseChannel)
    (continuation sendOutput)

runAPIServer ::
  forall tx.
  Tx tx =>
  IP ->
  PortNumber ->
  Tracer IO APIServerLog ->
  TVar [Either (ClientInput tx) (ServerOutput tx)] ->
  (ClientInput tx -> IO ()) ->
  TChan (ServerOutput tx) ->
  IO ()
runAPIServer host port tracer history recvInput responseChannel = do
  traceWith tracer (APIServerStarted port)
  runServer (show host) (fromIntegral port) $ \pending -> do
    con <- acceptRequest pending
    chan <- STM.atomically $ dupTChan responseChannel
    traceWith tracer NewAPIConnection
    forwardHistory con
    withPingThread con 30 (pure ()) $
      race_ (receiveRequests con) (sendOutputs chan con)
 where
  sendOutputs chan con = forever $ do
    response <- STM.atomically $ readTChan chan
    let sentResponse = Aeson.encode response
    sendTextData con sentResponse
    traceWith tracer (APIResponseSent sentResponse)

  receiveRequests con = forever $ do
    msg <- receiveData con
    case Aeson.eitherDecode msg of
      Right request -> do
        traceWith tracer (APIRequestReceived msg)
        recvInput request
      Left{} -> do
        sendTextData con $ Aeson.encode $ InvalidInput @tx
        traceWith tracer (APIInvalidRequest msg)

  forwardHistory con = do
    hist <- STM.atomically (readTVar history)
    let encodeAndReverse xs = \case
          Left clientOutput -> Aeson.encode clientOutput : xs
          Right serverOutput -> Aeson.encode serverOutput : xs
    sendTextDatas con $ foldl' encodeAndReverse [] hist
