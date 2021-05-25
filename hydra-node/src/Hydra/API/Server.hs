{-# LANGUAGE TypeApplications #-}

module Hydra.API.Server where

import Cardano.Prelude hiding (Option, option)
import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import qualified Data.Text as Text
import Hydra.Logic (
  ClientResponse,
  Environment (..),
 )
import Hydra.Node (
  HydraNode (..),
  handleClientRequest,
 )
import Network.WebSockets (acceptRequest, receiveData, runServer, sendTextData, withPingThread)

runAPIServer :: (Show tx, Read tx) => TChan (ClientResponse tx) -> HydraNode tx IO -> IO ()
runAPIServer responseChannel node = do
  logAPI $ "Listening on port " <> show port
  runServer "0.0.0.0" port $ \pending -> do
    con <- acceptRequest pending
    chan <- atomically $ dupTChan responseChannel
    logAPI "Accepted new connection"
    withPingThread con 30 (pure ()) $
      race_ (receiveRequests con) (sendResponses chan con)
 where
  sendResponses chan con = forever $ do
    response <- atomically $ readTChan chan
    sendTextData con (show @_ @Text response)

  receiveRequests con = forever $ do
    msg <- receiveData con
    case readMaybe (Text.unpack msg) of
      Just request -> do
        logAPI $ "Received request: " <> show request
        handleClientRequest node request
      Nothing -> logAPI $ "Invalid request: " <> msg

  nodeId = party $ env node

  port = fromIntegral $ 4000 + nodeId

  logAPI t = putText $ "[API:" <> show nodeId <> "] " <> t
