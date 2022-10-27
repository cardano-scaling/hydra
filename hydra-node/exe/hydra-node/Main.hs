{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude

import Hydra.API.Server (withAPIServer)
import Hydra.Cardano.Api (TxId, serialiseToRawBytesHex)
import Hydra.Chain (ChainCallback, ChainHandle)
import Hydra.Chain.Direct (withDirectChain)
import Hydra.Chain.Direct.Handlers (ChainStateAt)
import Hydra.Chain.Direct.ScriptRegistry (publishHydraScripts)
import Hydra.Chain.Direct.Util (readKeyPair, readVerificationKey)
import Hydra.HeadLogic (Environment (..), Event (..), defaultTTL)
import Hydra.Ledger.Cardano (Tx)
import qualified Hydra.Ledger.Cardano as Ledger
import Hydra.Ledger.Cardano.Configuration (
  newGlobals,
  newLedgerEnv,
  protocolParametersFromJson,
  readJsonFileThrow,
  shelleyGenesisFromJson,
 )
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Logging.Monitoring (withMonitoring)
import Hydra.Network (Host (..))
import Hydra.Network.Heartbeat (withHeartbeat)
import Hydra.Network.Ouroboros (withIOManager, withOuroborosNetwork)
import Hydra.Node (
  EventQueue (..),
  HydraNodeLog (..),
  Persistence,
  createEventQueue,
  createHydraNode,
  createPersistence,
  initEnvironment,
  runHydraNode,
 )
import Hydra.Options (
  ChainConfig (..),
  Command (Publish, Run),
  LedgerConfig (..),
  PublishOptions (..),
  RunOptions (..),
  explain,
  parseHydraCommand,
  validateRunOptions,
 )
import Hydra.Party (Party)

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
        traceWith (contramap Node tracer) (NodeOptions opts)
        eq <- createEventQueue
        let RunOptions{hydraScriptsTxId, chainConfig} = opts
        persistChainState <- createPersistence Proxy $ persistenceDir <> "/chainstate"
        withChain tracer party (putEvent eq . OnChainEvent) hydraScriptsTxId persistChainState chainConfig $ \oc -> do
          let RunOptions{host, port, peers, nodeId} = opts
          withNetwork (contramap Network tracer) host port peers nodeId (putEvent eq . NetworkEvent defaultTTL) $ \hn -> do
            let RunOptions{apiHost, apiPort} = opts
            withAPIServer apiHost apiPort party (contramap APIServer tracer) (putEvent eq . ClientEvent) $ \server -> do
              let RunOptions{ledgerConfig} = opts
              withCardanoLedger ledgerConfig $ \ledger -> do
                let tr = contramap Node tracer
                persistHeadState <- createPersistence Proxy $ persistenceDir <> "/headstate"
                node <- createHydraNode tr eq hn ledger oc server env persistHeadState
                runHydraNode tr node

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

withChain ::
  Tracer IO (HydraLog Tx net) ->
  Party ->
  ChainCallback Tx IO ->
  TxId ->
  Persistence ChainStateAt IO ->
  ChainConfig ->
  (ChainHandle Tx IO -> IO ()) ->
  IO ()
withChain tracer party callback hydraScriptsTxId persistence config action = do
  keyPair@(vk, _) <- readKeyPair cardanoSigningKey
  otherCardanoKeys <- mapM readVerificationKey cardanoVerificationKeys
  withIOManager $ \iocp -> do
    withDirectChain
      (contramap DirectChain tracer)
      networkId
      iocp
      nodeSocket
      keyPair
      party
      (vk : otherCardanoKeys)
      startChainFrom
      hydraScriptsTxId
      persistence
      callback
      action
 where
  DirectChainConfig{networkId, nodeSocket, cardanoSigningKey, cardanoVerificationKeys, startChainFrom} = config

identifyNode :: RunOptions -> RunOptions
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
