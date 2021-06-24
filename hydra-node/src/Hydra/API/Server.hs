{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hydra.API.Server (
  withAPIServer,
  APIServerLog,
) where

import Hydra.Prelude

import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (newBroadcastTChanIO, writeTChan)
import Data.Aeson (FromJSON, eitherDecode)
import Hydra.HeadLogic (
  ClientRequest,
  ClientResponse,
 )
import Hydra.Ledger (Tx (..))
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (IP, PortNumber)
import Network.WebSockets (acceptRequest, receiveData, runServer, sendTextData, withPingThread)

data APIServerLog
  = APIServerStarted {listeningPort :: PortNumber}
  | NewAPIConnection
  | APIResponseSent {sentResponse :: Text}
  | APIRequestReceived {receivedRequest :: LByteString}
  | APIInvalidRequest {receivedRequest :: LByteString}
  deriving (Eq, Show)

withAPIServer ::
  (Tx tx, FromJSON tx, FromJSON (UTxO tx)) =>
  IP ->
  PortNumber ->
  Tracer IO APIServerLog ->
  (ClientRequest tx -> IO ()) ->
  ((ClientResponse tx -> IO ()) -> IO ()) ->
  IO ()
withAPIServer host port tracer requests continuation = do
  responseChannel <- newBroadcastTChanIO
  let sendResponse = atomically . writeTChan responseChannel
  race_
    (runAPIServer host port tracer requests responseChannel)
    (continuation sendResponse)

runAPIServer ::
  (Tx tx, FromJSON tx, FromJSON (UTxO tx)) =>
  IP ->
  PortNumber ->
  Tracer IO APIServerLog ->
  (ClientRequest tx -> IO ()) ->
  TChan (ClientResponse tx) ->
  IO ()
runAPIServer host port tracer requestHandler responseChannel = do
  traceWith tracer (APIServerStarted port)
  runServer (show host) (fromIntegral port) $ \pending -> do
    con <- acceptRequest pending
    chan <- STM.atomically $ dupTChan responseChannel
    traceWith tracer NewAPIConnection
    withPingThread con 30 (pure ()) $
      race_ (receiveRequests con) (sendResponses chan con)
 where
  sendResponses chan con = forever $ do
    response <- STM.atomically $ readTChan chan
    let sentResponse = show response
    sendTextData con sentResponse
    traceWith tracer (APIResponseSent sentResponse)

  receiveRequests con = forever $ do
    msg <- receiveData con
    case eitherDecode msg of
      Right request -> do
        traceWith tracer (APIRequestReceived msg)
        requestHandler request
      Left{} ->
        traceWith tracer (APIInvalidRequest msg)
