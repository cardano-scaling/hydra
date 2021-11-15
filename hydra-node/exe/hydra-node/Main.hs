{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Hydra.Prelude

import Hydra.API.Server (withAPIServer)
import Hydra.Chain (Chain, ChainCallback)
import Hydra.Chain.Direct (withDirectChain)
import Hydra.Chain.Direct.Util (readKeyPair)
import Hydra.Chain.ZeroMQ (withMockChain)
import Hydra.HeadLogic (Environment (..), Event (..))
import Hydra.Ledger (Tx)
import qualified Hydra.Ledger.Cardano as Ledger
import Hydra.Logging (Tracer, Verbosity (..), nullTracer, withTracer)
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Logging.Monitoring (withMonitoring)
import Hydra.Network (Host (..))
import Hydra.Network.Heartbeat (withHeartbeat)
import Hydra.Network.Ouroboros (withIOManager, withOuroborosNetwork)
import Hydra.Node (
  EventQueue (..),
  createEventQueue,
  createHydraNode,
  initEnvironment,
  runHydraNode,
 )
import Hydra.Options (ChainConfig (..), Options (..), parseHydraOptions)
import Hydra.Party (Party)

main :: IO ()
main = do
  o@Options{verbosity, host, port, peers, apiHost, apiPort, monitoringPort, chainConfig} <- identifyNode <$> parseHydraOptions
  env@Environment{party} <- initEnvironment o
  withTracer verbosity $ \tracer' ->
    withMonitoring monitoringPort tracer' $ \tracer -> do
      eq <- createEventQueue
      withChain tracer party (putEvent eq . OnChainEvent) chainConfig $ \oc ->
        withNetwork (contramap Network tracer) host port peers (putEvent eq . NetworkEvent) $ \hn ->
          withAPIServer apiHost apiPort party (contramap APIServer tracer) (putEvent eq . ClientEvent) $ \server ->
            createHydraNode eq hn Ledger.cardanoLedger oc server env >>= runHydraNode (contramap Node tracer)
 where
  withNetwork tracer host port peers =
    let localhost = Host{hostname = show host, port}
     in withHeartbeat localhost $ withOuroborosNetwork tracer localhost peers

withChain ::
  Tx tx =>
  Tracer IO (HydraLog tx net) ->
  Party ->
  ChainCallback tx IO ->
  ChainConfig ->
  (Chain tx IO -> IO ()) ->
  IO ()
withChain tracer party callback config action = case config of
  MockChainConfig mockChain ->
    withMockChain (contramap MockChain tracer) mockChain callback action
  DirectChainConfig{networkMagic, nodeSocket, cardanoSigningKey} -> do
    keyPair <- readKeyPair cardanoSigningKey
    -- FIXME: cardano keys are not really used at this stage and they should be
    -- passed in the head parameters anyhow
    let cardanoKeys = []
    withIOManager $ \iocp -> do
      withDirectChain
        nullTracer -- FIXME: Enable logging for direct chain (require JSON for validated tx)
        networkMagic
        iocp
        nodeSocket
        keyPair
        party
        cardanoKeys
        callback
        action

identifyNode :: Options -> Options
identifyNode opt@Options{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
