{-# LANGUAGE TypeApplications #-}

module Main where

import Hydra.Prelude

import Hydra.API.Server (withAPIServer)
import Hydra.Chain.ZeroMQ (createMockChainClient)
import Hydra.HeadLogic (
  Environment (party),
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
  withTracer verbosity $ \tracer' ->
    withMonitoring monitoringPort tracer' $ \tracer -> do
      eq <- createEventQueue
      -- XXX(SN): this is soo weird, [] and mempty are both `parties`
      let headState = createHeadState [] (HeadParameters 10 mempty)
      hh <- createHydraHead headState Ledger.simpleLedger
      oc <- createMockChainClient eq (contramap MockChain tracer)
      withNetwork (contramap Network tracer) (party env) host port peers (putEvent eq . NetworkEvent) $
        \hn ->
          withAPIServer apiHost apiPort (contramap APIServer tracer) (putEvent eq . ClientEvent) $
            \sendOutput ->
              runHydraNode (contramap Node tracer) $ HydraNode{eq, hn, hh, oc, sendOutput, env}
 where
  withNetwork tracer party host port peers =
    let localhost = Host{hostName = show host, portNumber = port}
     in withBroadcastToSelf $ withHeartbeat party $ withOuroborosNetwork tracer localhost peers

identifyNode :: Options -> Options
identifyNode opt@Options{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
