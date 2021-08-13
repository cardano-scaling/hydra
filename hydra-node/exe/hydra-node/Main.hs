{-# LANGUAGE TypeApplications #-}

module Main where

import Hydra.Prelude

import Hydra.API.Server (withAPIServer)
import Hydra.Chain.ZeroMQ (withMockChain)
import Hydra.HeadLogic (
  Environment (party),
  Event (..),
  HeadState (ReadyState),
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
  o@Options{verbosity, host, port, peers, apiHost, apiPort, monitoringPort, mockChainPorts} <- identifyNode <$> parseHydraOptions
  env <- initEnvironment o
  withTracer verbosity $ \tracer' ->
    withMonitoring monitoringPort tracer' $ \tracer -> do
      eq <- createEventQueue
      hh <- createHydraHead ReadyState Ledger.simpleLedger
      withMockChain (contramap MockChain tracer) mockChainPorts (putEvent eq . OnChainEvent) $ \oc ->
        withNetwork (contramap Network tracer) (party env) host port peers (putEvent eq . NetworkEvent) $ \hn ->
          withAPIServer apiHost apiPort (contramap APIServer tracer) (putEvent eq . ClientEvent) $ \sendOutput ->
            runHydraNode (contramap Node tracer) $ HydraNode{eq, hn, hh, oc, sendOutput, env}
 where
  withNetwork tracer party host port peers =
    let localhost = Host{hostName = show host, portNumber = port}
     in withBroadcastToSelf $ withHeartbeat party $ withOuroborosNetwork tracer localhost peers

identifyNode :: Options -> Options
identifyNode opt@Options{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
