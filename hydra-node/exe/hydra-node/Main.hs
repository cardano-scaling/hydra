{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Prelude

import Data.Text (pack)
import qualified Data.Text as Text
import qualified Hydra.Ledger.Mock as Ledger
import Hydra.Logic (
  Environment (..),
  Event (..),
  HeadParameters (..),
  SnapshotStrategy (..),
  createHeadState,
 )
import Hydra.Network.ZeroMQ (
  withZeroMQHydraNetwork,
 )
import Hydra.Node (
  EventQueue (..),
  HydraNode (..),
  createEventQueue,
  createHydraHead,
  createMockChainClient,
  handleClientRequest,
  runHydraNode,
 )
import Network.WebSockets (acceptRequest, receiveData, runServer, withPingThread)

main :: IO ()
main = do
  [nodeId] <- getArgs
  case readMaybe nodeId of
    Nothing -> panic $ "invalid nodeId argument, should be a number: " <> pack nodeId
    Just n -> do
      let env = Environment n
      eq <- createEventQueue
      let headState = createHeadState [] HeadParameters SnapshotStrategy
      hh <- createHydraHead headState Ledger.mockLedger
      oc <- createMockChainClient env eq
      withZeroMQHydraNetwork (me n) (them n) (putEvent eq . NetworkEvent) $ \hn -> do
        let sendResponse = putText . ("Should send response: " <>) . show
        let node = HydraNode{eq, hn, hh, oc, sendResponse, env}
        race_
          (runAPIServer node)
          (runHydraNode node)
 where
  -- HACK(SN): Obviously we should configure the node instead
  me nodeId = ("127.0.0.1", show $ 5000 + nodeId)
  them nodeId = [("127.0.0.1", show $ 5000 + id) | id <- [1 .. 3], id /= nodeId]

runAPIServer :: (Show tx, Read tx) => HydraNode tx IO -> IO ()
runAPIServer node = do
  logAPI $ "Listening on port " <> show port
  runServer "0.0.0.0" port $ \pending -> do
    con <- acceptRequest pending
    logAPI "Accepted new connection"
    withPingThread con 30 (pure ()) $
      forever $ do
        msg <- receiveData con
        case readMaybe (Text.unpack msg) of
          Just request -> do
            logAPI $ "Received request: " <> show request
            handleClientRequest node request
          Nothing -> logAPI $ "Invalid request: " <> msg
 where
  nodeId = party $ env node
  port = fromIntegral $ 4000 + nodeId
  logAPI t = putText $ "[API:" <> show nodeId <> "] " <> t
