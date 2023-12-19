{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Node.Run where

import Hydra.Prelude hiding (fromList)

import Cardano.Ledger.Shelley.API qualified as Shelley
import Hydra.API.Server (Server (..), withAPIServer)
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Cardano.Api (
  ProtocolParametersConversionError,
  ShelleyBasedEra (..),
  toLedgerPParams,
 )
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Chain.CardanoClient (QueryPoint (..), queryGenesisParameters)
import Hydra.Chain.Direct (loadChainContext, mkTinyWallet, withDirectChain)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.Chain.Offline (loadGlobalsFromFile, withOfflineChain)
import Hydra.HeadLogic (
  Environment (..),
  Event (..),
  defaultTTL,
 )
import Hydra.Ledger.Cardano qualified as Ledger
import Hydra.Ledger.Cardano.Configuration (
  newGlobals,
  newLedgerEnv,
  protocolParametersFromJson,
  readJsonFileThrow,
 )
import Hydra.Logging (Verbosity (..), traceWith, withTracer)
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Logging.Monitoring (withMonitoring)
import Hydra.Network.Authenticate (Authenticated (Authenticated))
import Hydra.Network.Message (Connectivity (..))
import Hydra.Node (
  HydraNode (..),
  checkHeadState,
  createNodeState,
  initEnvironment,
  loadState,
  runHydraNode,
 )
import Hydra.Node.EventQueue (EventQueue (..), createEventQueue)
import Hydra.Node.Network (NetworkConfiguration (..), withNetwork)
import Hydra.Options (
  ChainConfig (..),
  InvalidOptions (..),
  LedgerConfig (..),
  RunOptions (..),
  validateRunOptions,
 )
import Hydra.Persistence (createPersistenceIncremental)

data ConfigurationException
  = ConfigurationException ProtocolParametersConversionError
  | InvalidOptionException InvalidOptions
  deriving stock (Show)

instance Exception ConfigurationException where
  displayException = \case
    InvalidOptionException MaximumNumberOfPartiesExceeded ->
      "Maximum number of parties is currently set to: " <> show maximumNumberOfParties
    InvalidOptionException CardanoAndHydraKeysMissmatch ->
      "Number of loaded cardano and hydra keys needs to match"
    ConfigurationException err ->
      "Incorrect protocol parameters configuration provided: " <> show err

run :: RunOptions -> IO ()
run opts = do
  either (throwIO . InvalidOptionException) pure $ validateRunOptions opts
  let RunOptions{verbosity, monitoringPort, persistenceDir} = opts
  env@Environment{party, otherParties, signingKey} <- initEnvironment opts
  withTracer verbosity $ \tracer' ->
    withMonitoring monitoringPort tracer' $ \tracer -> do
      traceWith tracer (NodeOptions opts)
      eq@EventQueue{putEvent} <- createEventQueue
      let RunOptions{chainConfig, ledgerConfig} = opts
      protocolParams <- readJsonFileThrow protocolParametersFromJson (cardanoLedgerProtocolParametersFile ledgerConfig)
      pparams <- case toLedgerPParams ShelleyBasedEraBabbage protocolParams of
        Left err -> throwIO (ConfigurationException err)
        Right bpparams -> pure bpparams
      globals <- getGlobalsForChain chainConfig
      withCardanoLedger pparams globals $ \ledger -> do
        persistence <- createPersistenceIncremental $ persistenceDir <> "/state"
        (hs, chainStateHistory) <- loadState (contramap Node tracer) persistence initialChainState

        checkHeadState (contramap Node tracer) env hs
        nodeState <- createNodeState hs
        -- Chain
        withChain <- prepareChainComponent tracer env globals chainConfig
        withChain chainStateHistory (putEvent . OnChainEvent) $ \chain -> do
          -- API
          let RunOptions{host, port, peers, nodeId} = opts
              putNetworkEvent (Authenticated msg otherParty) = putEvent $ NetworkEvent defaultTTL otherParty msg
              RunOptions{apiHost, apiPort} = opts
          apiPersistence <- createPersistenceIncremental $ persistenceDir <> "/server-output"
          withAPIServer apiHost apiPort party apiPersistence (contramap APIServer tracer) chain pparams (putEvent . ClientEvent) $ \server -> do
            -- Network
            let networkConfiguration = NetworkConfiguration{persistenceDir, signingKey, otherParties, host, port, peers, nodeId}
            withNetwork tracer (connectionMessages server) networkConfiguration putNetworkEvent $ \hn -> do
              -- Main loop
              runHydraNode (contramap Node tracer) $
                HydraNode
                  { eq
                  , hn
                  , nodeState
                  , oc = chain
                  , server
                  , ledger
                  , env
                  , persistence
                  }
 where
  connectionMessages Server{sendOutput} = \case
    Connected nodeid -> sendOutput $ PeerConnected nodeid
    Disconnected nodeid -> sendOutput $ PeerDisconnected nodeid

  withCardanoLedger protocolParams globals action =
    let ledgerEnv = newLedgerEnv protocolParams
     in action (Ledger.cardanoLedger globals ledgerEnv)

  prepareChainComponent tracer Environment{party} globals chainConfig =
    case chainConfig of
      OfflineChainConfig{} ->
        -- TODO: rename DirectChain to just "chain"
        pure $ withOfflineChain (contramap DirectChain tracer) chainConfig globals party
      DirectChainConfig{} -> do
        ctx <- loadChainContext chainConfig party
        wallet <- mkTinyWallet (contramap DirectChain tracer) chainConfig
        pure $ withDirectChain (contramap DirectChain tracer) chainConfig ctx wallet

getGlobalsForChain :: ChainConfig -> IO Shelley.Globals
getGlobalsForChain = \case
  OfflineChainConfig{ledgerGenesisFile} ->
    loadGlobalsFromFile ledgerGenesisFile
  DirectChainConfig{networkId, nodeSocket} ->
    queryGenesisParameters networkId nodeSocket QueryTip
      >>= newGlobals

identifyNode :: RunOptions -> RunOptions
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
