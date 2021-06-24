{-# LANGUAGE TypeApplications #-}

module Main where

import Hydra.Prelude

import Hydra.API.Server (withAPIServer)
import Hydra.Chain.ZeroMQ (createMockChainClient)
import Hydra.HeadLogic (
  Event (..),
  HeadParameters (..),
  createHeadState,
 )
import qualified Hydra.Ledger.Simple as Ledger
import Hydra.Logging (Verbosity (..), withTracer)
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Logging.Monitoring (withMonitoring)
import Hydra.Network (Host (..))
import Hydra.Network.BroadcastToSelf (withBroadcastToSelf)
import Hydra.Network.Heartbeat (withHeartbeat)
import Hydra.Network.Ouroboros (withOuroborosNetwork)
import Hydra.Node (
  EventQueue (..),
  HydraNode (..),
  createEventQueue,
  createHydraHead,
  initEnvironment,
  runHydraNode,
 )
import Hydra.Options (Options (..), parseHydraOptions)

main :: IO ()
main = do
  o@Options{verbosity, host, port, peers, apiHost, apiPort, monitoringPort} <- identifyNode <$> parseHydraOptions
  env <- initEnvironment o
  withTracer verbosity show $ \tracer' ->
    withMonitoring monitoringPort tracer' $ \tracer -> do
      eq <- createEventQueue
      let headState = createHeadState [] (HeadParameters 3 mempty)
      hh <- createHydraHead headState Ledger.simpleLedger
      oc <- createMockChainClient eq (contramap MockChain tracer)
      withNetwork (contramap Network tracer) host port peers (putEvent eq . NetworkEvent) $
        \hn ->
          withAPIServer apiHost apiPort (contramap APIServer tracer) (putEvent eq . ClientEvent) $
            \sendResponse ->
              runHydraNode (contramap Node tracer) $ HydraNode{eq, hn, hh, oc, sendResponse, env}
 where
  withNetwork tracer host port peers =
    let localhost = Host{hostName = show host, portNumber = port}
     in withHeartbeat localhost $ withBroadcastToSelf $ withOuroborosNetwork tracer localhost peers

identifyNode :: Options -> Options
identifyNode opt@Options{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
