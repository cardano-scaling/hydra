{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Hydra.Prelude

import Hydra.API.Server (withAPIServer)
import Hydra.Cardano.Api (serialiseToRawBytesHex)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct (initialChainState, loadChainContext, mkTinyWallet, withDirectChain)
import Hydra.Chain.Direct.ScriptRegistry (publishHydraScripts)
import Hydra.Chain.Direct.State (ChainStateAt (..))
import Hydra.Chain.Direct.Util (readKeyPair)
import Hydra.HeadLogic (
  ClosedState (..),
  Environment (..),
  Event (..),
  HeadState (..),
  IdleState (..),
  InitialState (..),
  OpenState (..),
  defaultTTL,
  getChainState,
 )
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
  chainCallback,
  createEventQueue,
  createNodeState,
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
import Hydra.Persistence (Persistence (load), PersistenceException (..), createPersistence, createPersistenceIncremental)

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
    env@Environment{party, otherParties} <- initEnvironment opts
    withTracer verbosity $ \tracer' ->
      withMonitoring monitoringPort tracer' $ \tracer -> do
        traceWith tracer (NodeOptions opts)
        eq <- createEventQueue
        let RunOptions{hydraScriptsTxId, chainConfig} = opts
        -- Load state from persistence or create new one
        persistence <- createPersistence $ persistenceDir <> "/state"
        hs <-
          load persistence >>= \case
            Nothing -> do
              traceWith tracer CreatedState
              pure $ Idle IdleState{chainState = initialChainState}
            Just headState -> do
              traceWith tracer LoadedState
              let paramsMismatch = checkParamsAgainstExistingState headState env
              when (not $ null paramsMismatch) $ do
                traceWith tracer (Misconfiguration paramsMismatch)
                throwIO $ PersistenceException $ concat paramsMismatch
              pure headState
        nodeState <- createNodeState hs
        ctx <- loadChainContext chainConfig party otherParties hydraScriptsTxId
        wallet <- mkTinyWallet (contramap DirectChain tracer) chainConfig
        let ChainStateAt{recordedAt} = getChainState hs
        withDirectChain (contramap DirectChain tracer) chainConfig ctx recordedAt wallet (chainCallback nodeState eq) $ \chain -> do
          let RunOptions{host, port, peers, nodeId} = opts
          withNetwork (contramap Network tracer) host port peers nodeId (putEvent eq . NetworkEvent defaultTTL) $ \hn -> do
            let RunOptions{apiHost, apiPort} = opts

            apiPersistence <- createPersistenceIncremental $ persistenceDir <> "/server-output"
            withAPIServer apiHost apiPort party apiPersistence (contramap APIServer tracer) (putEvent eq . ClientEvent) $ \server -> do
              let RunOptions{ledgerConfig} = opts
              withCardanoLedger ledgerConfig $ \ledger ->
                runHydraNode (contramap Node tracer) $
                  HydraNode{eq, hn, nodeState, oc = chain, server, ledger, env, persistence}

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

  -- check if hydra-node parameters are matching with the hydra-node state.
  checkParamsAgainstExistingState :: HeadState Ledger.Tx -> Environment -> [String]
  checkParamsAgainstExistingState hs env =
    case hs of
      Idle _ -> []
      Initial InitialState{parameters} -> validateParameters "InitialState: " parameters
      Open OpenState{parameters} -> validateParameters "OpenState: " parameters
      Closed ClosedState{parameters} -> validateParameters "ClosedState: " parameters
   where
    validateParameters st params =
      let res = flip execState [] $ do
            when (Hydra.Chain.contestationPeriod params /= cp) $
              modify (\s -> s <> ["Contestation period does not match. "])
            when (sort (Hydra.Chain.parties params) /= sort envParties) $
              modify (\s -> s <> ["Parties mismatch. "])
       in case res of
            [] -> []
            items -> st : items

    Environment{contestationPeriod = cp, otherParties, party} = env
    envParties = party : otherParties

identifyNode :: RunOptions -> RunOptions
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
