{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude

import Hydra.API.Server (Server (Server, sendOutput), withAPIServer)
import Hydra.API.ServerOutput (ServerOutput (PeerConnected, PeerDisconnected))
import Hydra.Cardano.Api (serialiseToRawBytesHex)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.CardanoClient (QueryPoint (..), queryGenesisParameters)
import Hydra.Chain.Direct (initialChainState, loadChainContext, mkTinyWallet, withDirectChain)
import Hydra.Chain.Direct.ScriptRegistry (publishHydraScripts)
import Hydra.Chain.Direct.Util (readKeyPair)
import Hydra.HeadLogic (getChainState)
import Hydra.HeadLogicTypes (
  ClosedState (..),
  Environment (..),
  Event (..),
  HeadState (..),
  IdleState (..),
  InitialState (..),
  OpenState (..),
  defaultTTL,
 )
import qualified Hydra.Ledger.Cardano as Ledger
import Hydra.Ledger.Cardano.Configuration (
  newGlobals,
  newLedgerEnv,
  protocolParametersFromJson,
  readJsonFileThrow,
 )
import Hydra.Logging (Verbosity (..), traceWith, withTracer)
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Logging.Monitoring (withMonitoring)
import Hydra.Network (Host (..))
import Hydra.Network.Heartbeat (withHeartbeat)
import Hydra.Network.Message (Connectivity (..))
import Hydra.Network.Ouroboros (withOuroborosNetwork)
import Hydra.Node (
  HydraNode (..),
  createNodeState,
  initEnvironment,
  runHydraNode,
 )
import Hydra.Node.EventQueue (EventQueue (..), createEventQueue)
import Hydra.Options (
  ChainConfig (..),
  Command (Publish, Run),
  LedgerConfig (..),
  ParamMismatch (..),
  PublishOptions (..),
  RunOptions (..),
  explain,
  parseHydraCommand,
  validateRunOptions,
 )
import Hydra.Persistence (Persistence (load), createPersistence, createPersistenceIncremental)

newtype ParamMismatchError = ParamMismatchError String deriving (Eq, Show)

instance Exception ParamMismatchError

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
        eq@EventQueue{putEvent} <- createEventQueue
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
              unless (null paramsMismatch) $ do
                traceWith tracer (Misconfiguration paramsMismatch)
                throwIO $
                  ParamMismatchError $
                    "Loaded state does not match given command line options."
                      <> " Please check the state in: "
                      <> persistenceDir
                      <> " against provided command line options."
              pure headState
        nodeState <- createNodeState hs
        ctx <- loadChainContext chainConfig party otherParties hydraScriptsTxId
        wallet <- mkTinyWallet (contramap DirectChain tracer) chainConfig
        withDirectChain (contramap DirectChain tracer) chainConfig ctx wallet (getChainState hs) (putEvent . OnChainEvent) $ \chain -> do
          let RunOptions{host, port, peers, nodeId} = opts
              putNetworkEvent = putEvent . NetworkEvent defaultTTL
              RunOptions{apiHost, apiPort} = opts
          apiPersistence <- createPersistenceIncremental $ persistenceDir <> "/server-output"
          withAPIServer apiHost apiPort party apiPersistence (contramap APIServer tracer) chain (putEvent . ClientEvent) $ \server -> do
            withNetwork (contramap Network tracer) server host port peers nodeId putNetworkEvent $ \hn -> do
              let RunOptions{ledgerConfig} = opts
              withCardanoLedger ledgerConfig chainConfig $ \ledger ->
                runHydraNode (contramap Node tracer) $
                  HydraNode{eq, hn, nodeState, oc = chain, server, ledger, env, persistence}

  publish opts = do
    (_, sk) <- readKeyPair (publishSigningKey opts)
    let PublishOptions{publishNetworkId = networkId, publishNodeSocket = nodeSocket} = opts
    txId <- publishHydraScripts networkId nodeSocket sk
    putStrLn (decodeUtf8 (serialiseToRawBytesHex txId))

  withNetwork tracer Server{sendOutput} host port peers nodeId =
    let localhost = Host{hostname = show host, port}
        connectionMessages = \case
          Connected nodeid -> sendOutput $ PeerConnected nodeid
          Disconnected nodeid -> sendOutput $ PeerDisconnected nodeid
     in withHeartbeat nodeId connectionMessages $ withOuroborosNetwork tracer localhost peers

  withCardanoLedger ledgerConfig chainConfig action = do
    let DirectChainConfig{networkId, nodeSocket} = chainConfig
    globals <- newGlobals =<< queryGenesisParameters networkId nodeSocket QueryTip
    ledgerEnv <-
      newLedgerEnv
        <$> readJsonFileThrow protocolParametersFromJson (cardanoLedgerProtocolParametersFile ledgerConfig)

    action (Ledger.cardanoLedger globals ledgerEnv)

  -- check if hydra-node parameters are matching with the hydra-node state.
  checkParamsAgainstExistingState :: HeadState Ledger.Tx -> Environment -> [ParamMismatch]
  checkParamsAgainstExistingState hs env =
    case hs of
      Idle _ -> []
      Initial InitialState{parameters} -> validateParameters parameters
      Open OpenState{parameters} -> validateParameters parameters
      Closed ClosedState{parameters} -> validateParameters parameters
   where
    validateParameters HeadParameters{contestationPeriod = loadedCp, parties} =
      flip execState [] $ do
        when (loadedCp /= configuredCp) $
          modify (<> [ContestationPeriodMismatch{loadedCp, configuredCp}])
        when (loadedParties /= configuredParties) $
          modify (<> [PartiesMismatch{loadedParties, configuredParties}])
     where
      loadedParties = sort parties

    Environment{contestationPeriod = configuredCp, otherParties, party} = env
    configuredParties = sort (party : otherParties)

identifyNode :: RunOptions -> RunOptions
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
