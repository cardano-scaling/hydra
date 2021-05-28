{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Prelude hiding (Option, option)

import Control.Concurrent.STM (newBroadcastTChanIO, writeTChan)
import Hydra.API.Server (APIServerLog, runAPIServer)
import Hydra.Ledger.Mock (MockTx)
import qualified Hydra.Ledger.Mock as Ledger
import Hydra.Logging
import Hydra.Logic (
  Environment (..),
  Event (..),
  HeadParameters (..),
  SnapshotStrategy (..),
  createHeadState,
 )
import Hydra.MockZMQChain (MockChainLog)
import Hydra.Network.ZeroMQ (
  NetworkLog,
  withZeroMQHydraNetwork,
 )
import Hydra.Node (
  EventQueue (..),
  HydraNode (..),
  HydraNodeLog,
  createEventQueue,
  createHydraHead,
  createMockChainClient,
  runHydraNode,
 )
import Hydra.Option (Option (..), parseHydraOptions)

data HydraLog
  = MockChain MockChainLog
  | APIServer APIServerLog
  | Network NetworkLog
  | Node (HydraNodeLog MockTx)
  deriving (Show)

main :: IO ()
main = do
  Option{nodeId, verbosity, host, port, peers, apiHost, apiPort, monitoringPort} <- identifyNode <$> parseHydraOptions
  withTracer monitoringPort verbosity show $ \tracer -> do
    let env = Environment nodeId
    eq <- createEventQueue
    let headState = createHeadState [] HeadParameters SnapshotStrategy
    hh <- createHydraHead headState Ledger.mockLedger
    oc <- createMockChainClient eq (contramap (map MockChain) tracer)
    withZeroMQHydraNetwork (show host, port) peers (contramap (Log . Network) tracer) (putEvent eq . NetworkEvent) $ \hn -> do
      responseChannel <- newBroadcastTChanIO
      let sendResponse = atomically . writeTChan responseChannel
      let node = HydraNode{eq, hn, hh, oc, sendResponse, env}
      race_
        (runAPIServer apiHost apiPort responseChannel node (contramap (map APIServer) tracer))
        (runHydraNode (contramap (map Node) tracer) node)

identifyNode :: Option -> Option
identifyNode opt@Option{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
