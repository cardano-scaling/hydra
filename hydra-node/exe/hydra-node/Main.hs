{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Hydra.Prelude

import Hydra.API.Server (withAPIServer)
import Hydra.Chain.ZeroMQ (withMockChain)
import Hydra.HeadLogic (Event (..))
import qualified Hydra.Ledger.Cardano as Ledger
import Hydra.Logging (Verbosity (..), withTracer)
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Logging.Monitoring (withMonitoring)
import Hydra.Network (Host (..))
import Hydra.Network.Heartbeat (withHeartbeat)
import Hydra.Network.Ouroboros (withOuroborosNetwork)
import Hydra.Node (
  EventQueue (..),
  createEventQueue,
  createHydraNode,
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
      withMockChain (contramap MockChain tracer) mockChainPorts (putEvent eq . OnChainEvent) $ \oc ->
        withNetwork (contramap Network tracer) host port peers (putEvent eq . NetworkEvent) $ \hn ->
          withAPIServer apiHost apiPort (contramap APIServer tracer) (putEvent eq . ClientEvent) $ \server ->
            createHydraNode eq hn Ledger.cardanoLedger oc server env >>= runHydraNode (contramap Node tracer)
 where
  withNetwork tracer host port peers =
    let localhost = Host{hostname = show host, port}
     in withHeartbeat localhost $ withOuroborosNetwork tracer localhost peers

identifyNode :: Options -> Options
identifyNode opt@Options{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
