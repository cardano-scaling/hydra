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
import Hydra.Network (Port (Port))
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
  Option{nodeId, verbosity, host, port} <- identifyNode <$> parseHydraOptions
  withTracer verbosity show $ \tracer -> do
    let env = Environment nodeId
    eq <- createEventQueue
    let headState = createHeadState [] HeadParameters SnapshotStrategy
    hh <- createHydraHead headState Ledger.mockLedger
    oc <- createMockChainClient eq (contramap MockChain tracer)
    withZeroMQHydraNetwork (me host port) (them nodeId host) (contramap Network tracer) (putEvent eq . NetworkEvent) $ \hn -> do
      responseChannel <- newBroadcastTChanIO
      let sendResponse = atomically . writeTChan responseChannel
      let node = HydraNode{eq, hn, hh, oc, sendResponse, env}
      race_
        (runAPIServer responseChannel node (contramap APIServer tracer))
        (runHydraNode (contramap Node tracer) node)
 where
  me host port = (show host, port)
  them nodeId host = [(show host, Port $ fromIntegral $ 5000 + id) | id <- [1 .. 3], id /= nodeId]

identifyNode :: Option -> Option
identifyNode opt@Option{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
