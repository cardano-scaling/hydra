{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Node.Run where

import Hydra.Prelude hiding (fromList)

import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Crypto qualified as Ledger
import Cardano.Ledger.Shelley.API qualified as Shelley
import Hydra.API.Server (Server (..), withAPIServer)
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Cardano.Api (
  GenesisParameters (..),
  ProtocolParametersConversionError,
  ShelleyBasedEra (..),
  toLedgerPParams,
 )
import Hydra.Cardano.Api qualified as Shelley
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Chain.CardanoClient (QueryPoint (..), queryGenesisParameters)
import Hydra.Chain.Direct (loadChainContext, mkTinyWallet, withDirectChain)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.Chain.Offline (withOfflineChain, loadGlobalsFromGenesis, loadState)
import Hydra.HeadId (HeadId (..))
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
  initEnvironmentOffline,
  runHydraNode,
 )
import Hydra.Node.EventQueue (EventQueue (..), createEventQueue)
import Hydra.Node.Network (NetworkConfiguration (..), withNetwork)
import Hydra.Options (
  ChainConfig (..),
  InvalidOptions (..),
  LedgerConfig (..),
  OfflineConfig (..),
  RunOptions (..),
  RunOfflineOptions (..),
  validateRunOptions
 )
import Hydra.Options.Offline qualified as OfflineOptions
import Hydra.Options.Online qualified as OnlineOptions
import Hydra.Persistence (createPersistenceIncremental)
import Hydra.Network (NodeId(NodeId))


data ConfigurationException
  = ConfigurationException ProtocolParametersConversionError
  | InvalidOptionException InvalidOptions
  deriving stock (Show)
  deriving anyclass (Exception)

explain :: ConfigurationException -> String
explain = \case
  InvalidOptionException MaximumNumberOfPartiesExceeded ->
    "Maximum number of parties is currently set to: " <> show maximumNumberOfParties
  InvalidOptionException CardanoAndHydraKeysMissmatch ->
    "Number of loaded cardano and hydra keys needs to match"
  ConfigurationException err ->
    "Incorrect protocol parameters configuration provided: " <> show err

runOffline :: RunOfflineOptions -> IO ()
runOffline opts = do
  either (throwIO . InvalidOptionException) pure $ OfflineOptions.validateRunOfflineOptions opts
  let RunOfflineOptions{verbosity, monitoringPort, persistenceDir, offlineConfig} = opts
  env@Environment{party, otherParties, signingKey, contestationPeriod} <- initEnvironmentOffline opts

  withTracer verbosity $ \tracer' ->
    withMonitoring monitoringPort tracer' $ \tracer -> do
      traceWith tracer (NodeOfflineOptions opts)
      eq@EventQueue{putEvent} <- createEventQueue
      let RunOfflineOptions{ledgerConfig} = opts
      protocolParams <- readJsonFileThrow protocolParametersFromJson (cardanoLedgerProtocolParametersFile ledgerConfig)
      pparams <- case toLedgerPParams ShelleyBasedEraBabbage protocolParams of
        Left err -> throwIO (ConfigurationException err)
        Right bpparams -> pure bpparams
      -- let DirectChainConfig{networkId, nodeSocket} = chainConfig

      globals <- loadGlobalsFromGenesis (ledgerGenesisFile offlineConfig)

      withCardanoLedger pparams globals $ \ledger -> do
        persistence <- createPersistenceIncremental $ persistenceDir <> "/state"
        (hs, chainStateHistory) <- loadState (contramap Node tracer) persistence initialChainState

        checkHeadState (contramap Node tracer) env hs
        nodeState <- createNodeState hs
        -- Chain
        let withChain cont =
                let headId = UnsafeHeadId "HeadId"
                 in withOfflineChain (contramap DirectChain tracer) offlineConfig globals headId party contestationPeriod chainStateHistory (putEvent . OnChainEvent) cont
        withChain $ \chain -> do
          -- API
          let RunOfflineOptions{host, port} = opts
              peers = []
              nodeId = NodeId "offline"
              putNetworkEvent (Authenticated msg otherParty) = putEvent $ NetworkEvent defaultTTL otherParty msg
              RunOfflineOptions{apiHost, apiPort} = opts
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

run :: RunOptions -> IO ()
run opts = do
  either (throwIO . InvalidOptionException) pure $ validateRunOptions opts
  let RunOptions{verbosity, monitoringPort, persistenceDir} = opts
  env@Environment{party, otherParties, signingKey, contestationPeriod} <- initEnvironment opts
  withTracer verbosity $ \tracer' ->
    withMonitoring monitoringPort tracer' $ \tracer -> do
      traceWith tracer (NodeOptions opts)
      eq@EventQueue{putEvent} <- createEventQueue
      let RunOptions{hydraScriptsTxId, chainConfig, ledgerConfig} = opts
      protocolParams <- readJsonFileThrow protocolParametersFromJson (cardanoLedgerProtocolParametersFile ledgerConfig)
      pparams <- case toLedgerPParams ShelleyBasedEraBabbage protocolParams of
        Left err -> throwIO (ConfigurationException err)
        Right bpparams -> pure bpparams

      let DirectChainConfig{networkId, nodeSocket} = chainConfig

      globals <- newGlobals =<< queryGenesisParameters networkId nodeSocket QueryTip

      withCardanoLedger pparams globals $ \ledger -> do
        persistence <- createPersistenceIncremental $ persistenceDir <> "/state"
        (hs, chainStateHistory) <- loadState (contramap Node tracer) persistence initialChainState

        checkHeadState (contramap Node tracer) env hs
        nodeState <- createNodeState hs
        -- Chain
        let withChain cont = do
                ctx <- loadChainContext chainConfig party hydraScriptsTxId
                wallet <- mkTinyWallet (contramap DirectChain tracer) chainConfig
                withDirectChain (contramap DirectChain tracer) chainConfig ctx wallet chainStateHistory (putEvent . OnChainEvent) cont
        withChain $ \chain -> do
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

identifyNode :: RunOptions -> RunOptions
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{OnlineOptions.verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt

-- TODO: export from cardano-api

-- | Taken from Cardano.Api.GenesisParameters, a private module in cardano-api
fromShelleyGenesis :: Shelley.ShelleyGenesis Ledger.StandardCrypto -> GenesisParameters Shelley.ShelleyEra
fromShelleyGenesis
  sg@Shelley.ShelleyGenesis
    { Shelley.sgSystemStart
    , Shelley.sgNetworkMagic
    , Shelley.sgActiveSlotsCoeff
    , Shelley.sgSecurityParam
    , Shelley.sgEpochLength
    , Shelley.sgSlotsPerKESPeriod
    , Shelley.sgMaxKESEvolutions
    , Shelley.sgSlotLength
    , Shelley.sgUpdateQuorum
    , Shelley.sgMaxLovelaceSupply
    , Shelley.sgGenDelegs = _ -- unused, might be of interest
    , Shelley.sgInitialFunds = _ -- unused, not retained by the node
    , Shelley.sgStaking = _ -- unused, not retained by the node
    } =
    GenesisParameters
      { protocolParamSystemStart = sgSystemStart
      , protocolParamNetworkId = Shelley.fromNetworkMagic $ Shelley.NetworkMagic sgNetworkMagic
      , protocolParamActiveSlotsCoefficient =
          Ledger.unboundRational
            sgActiveSlotsCoeff
      , protocolParamSecurity = fromIntegral sgSecurityParam
      , protocolParamEpochLength = sgEpochLength
      , protocolParamSlotLength = Shelley.fromNominalDiffTimeMicro sgSlotLength
      , protocolParamSlotsPerKESPeriod = fromIntegral sgSlotsPerKESPeriod
      , protocolParamMaxKESEvolutions = fromIntegral sgMaxKESEvolutions
      , protocolParamUpdateQuorum = fromIntegral sgUpdateQuorum
      , protocolParamMaxLovelaceSupply =
          Shelley.Lovelace
            (fromIntegral sgMaxLovelaceSupply)
      , protocolInitialUpdateableProtocolParameters = Shelley.sgProtocolParams sg
      }
