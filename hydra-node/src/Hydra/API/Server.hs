{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hydra.API.Server where

import Cardano.Prelude hiding (Option, option)
import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import qualified Data.Text as Text
import Hydra.Logging (Log, Tracer, traceEvent)
import Hydra.Logic (
  ClientResponse,
 )
import Hydra.Network (IP, PortNumber)
import Hydra.Node (
  HydraNode (..),
  handleClientRequest,
 )
import Network.WebSockets (acceptRequest, receiveData, runServer, sendTextData, withPingThread)

data APIServerLog
  = APIServerStarted {listeningPort :: PortNumber}
  | NewAPIConnection
  | APIResponseSent {sentResponse :: Text}
  | APIRequestReceived {receivedRequest :: Text}
  | APIInvalidRequest {receivedRequest :: Text}
  deriving (Show)

runAPIServer :: (Show tx, Read tx) => IP -> PortNumber -> TChan (ClientResponse tx) -> HydraNode tx IO -> Tracer IO (Log APIServerLog) -> IO ()
runAPIServer host port responseChannel node tracer = do
  traceEvent tracer (APIServerStarted port)
  runServer (show host) (fromIntegral port) $ \pending -> do
    con <- acceptRequest pending
    chan <- atomically $ dupTChan responseChannel
    traceEvent tracer NewAPIConnection
    withPingThread con 30 (pure ()) $
      race_ (receiveRequests con) (sendResponses chan con)
 where
  sendResponses chan con = forever $ do
    response <- atomically $ readTChan chan
    let sentResponse = show @_ @Text response
    sendTextData con sentResponse
    traceEvent tracer (APIResponseSent sentResponse)

  receiveRequests con = forever $ do
    msg <- receiveData con
    case readMaybe (Text.unpack msg) of
      Just request -> do
        traceEvent tracer (APIRequestReceived msg)
        handleClientRequest node request
      Nothing -> traceEvent tracer (APIInvalidRequest msg)
