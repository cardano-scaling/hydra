{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude

import Hydra.API.Server (withAPIServer)
import Hydra.Cardano.Api (serialiseToRawBytesHex)
import Hydra.Chain (ChainCallback)
import Hydra.Chain.Direct (initialChainState, withDirectChain)
import Hydra.Chain.Direct.ScriptRegistry (publishHydraScripts)
import Hydra.Chain.Direct.Util (readKeyPair)
import Hydra.HeadLogic (Environment (..), Event (..), HeadState (..), defaultTTL, getChainState)
import Hydra.Ledger.Cardano (Tx)
import qualified Hydra.Ledger.Cardano as Ledger
import Hydra.Ledger.Cardano.Configuration (
  newGlobals,
  newLedgerEnv,
  protocolParametersFromJson,
  readJsonFileThrow,
  shelleyGenesisFromJson,
 )
import Hydra.Logging (Verbosity (..), traceWith, withTracer)
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Logging.Monitoring (withMonitoring)
import Hydra.Network (Host (..))
import Hydra.Network.Heartbeat (withHeartbeat)
import Hydra.Network.Ouroboros (withOuroborosNetwork)
import Hydra.Node (
  EventQueue (..),
  HydraNode (..),
  NodeState (..),
  Persistence (load),
  createEventQueue,
  createNodeState,
  createPersistence,
  initEnvironment,
  runHydraNode,
 )
import Hydra.Options (
  Command (Publish, Run),
  LedgerConfig (..),
  PublishOptions (..),
  RunOptions (..),
  explain,
  parseHydraCommand,
  validateRunOptions,
 )

main :: IO ()
main = do
  command <- parseHydraCommand
  case command of
    Run options -> do
      either (die . explain) pure $ validateRunOptions options
      run (identifyNode options)
    Publish options ->
      publish options
 where
  run opts = do
    let RunOptions{verbosity, monitoringPort, persistenceDir} = opts
    env@Environment{party} <- initEnvironment opts
    withTracer verbosity $ \tracer' ->
      withMonitoring monitoringPort tracer' $ \tracer -> do
        traceWith tracer (NodeOptions opts)
        eq <- createEventQueue
        let RunOptions{hydraScriptsTxId, chainConfig} = opts
        -- Load state from persistence or create new one
        persistence <- createPersistence Proxy $ persistenceDir <> "/state"
        hs <-
          load persistence >>= \case
            Nothing -> do
              traceWith tracer CreatedState
              chainState <- initialChainState chainConfig party hydraScriptsTxId
              pure IdleState{chainState}
            Just a -> do
              traceWith tracer LoadedState
              pure a
        nodeState <- createNodeState hs

        withDirectChain (contramap DirectChain tracer) chainConfig (chainCallback nodeState eq) $ \chain -> do
          let RunOptions{host, port, peers, nodeId} = opts
          withNetwork (contramap Network tracer) host port peers nodeId (putEvent eq . NetworkEvent defaultTTL) $ \hn -> do
            let RunOptions{apiHost, apiPort} = opts
            withAPIServer apiHost apiPort party (contramap APIServer tracer) (putEvent eq . ClientEvent) $ \server -> do
              let RunOptions{ledgerConfig} = opts
              withCardanoLedger ledgerConfig $ \ledger ->
                runHydraNode (contramap Node tracer) $
                  HydraNode{eq, hn, nodeState, oc = chain, server, ledger, env, persistence}

  chainCallback :: NodeState Tx IO -> EventQueue IO (Event Tx) -> ChainCallback Tx IO
  chainCallback NodeState{queryHeadState} eq cont = do
    st <- atomically $ queryHeadState
    case cont $ getChainState st of
      Nothing -> pure ()
      Just chainEvent ->
        putEvent eq $ OnChainEvent{chainEvent}

  publish opts = do
    (_, sk) <- readKeyPair (publishSigningKey opts)
    let PublishOptions{publishNetworkId = networkId, publishNodeSocket = nodeSocket} = opts
    txId <- publishHydraScripts networkId nodeSocket sk
    putStrLn (decodeUtf8 (serialiseToRawBytesHex txId))

  withNetwork tracer host port peers nodeId =
    let localhost = Host{hostname = show host, port}
     in withHeartbeat nodeId $ withOuroborosNetwork tracer localhost peers

  withCardanoLedger ledgerConfig action = do
    globals <-
      newGlobals
        <$> readJsonFileThrow shelleyGenesisFromJson (cardanoLedgerGenesisFile ledgerConfig)

    ledgerEnv <-
      newLedgerEnv
        <$> readJsonFileThrow protocolParametersFromJson (cardanoLedgerProtocolParametersFile ledgerConfig)

    action (Ledger.cardanoLedger globals ledgerEnv)

identifyNode :: RunOptions -> RunOptions
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
