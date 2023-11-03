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
  StandardCrypto,
  SystemStart (SystemStart),
  Tx,
  toLedgerPParams,
 )
import Hydra.Cardano.Api qualified as Shelley
import Hydra.Chain (ChainEvent (..), OnChainTx (..), initHistory, maximumNumberOfParties)
import Hydra.Chain.CardanoClient (QueryPoint (..), queryGenesisParameters)
import Hydra.Chain.Direct (loadChainContext, mkTinyWallet, withDirectChain)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.Chain.Offline (withOfflineChain)
import Hydra.Chain.Offline.Persistence (createStateChangePersistence, initializeStateIfOffline)
import Hydra.HeadLogic (
  Environment (..),
  Event (..),
  StateChanged (..),
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
  OfflineConfig (OfflineConfig, initialUTxOFile, ledgerGenesisFile, utxoWriteBack),
  OfflineUTxOWriteBackConfig (..),
  RunOptions (..),
  offlineOptionsNormalizedUtxoWriteBackFilePath,
  validateRunOptions,
 )
import Hydra.Persistence (createPersistenceIncremental)

import Hydra.HeadId (HeadId (..))

import Data.Aeson qualified as Aeson
import Hydra.Chain.Direct.Fixture (defaultGlobals)
import Hydra.ContestationPeriod (fromChain)
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime)
import Hydra.Ledger (IsTx (UTxOType))
import Hydra.Persistence (PersistenceIncremental (PersistenceIncremental, append, loadAll))
import Hydra.Snapshot (Snapshot (Snapshot), utxo)
import UnliftIO.IO.File (writeBinaryFileDurableAtomic)

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

run :: RunOptions -> IO ()
run opts = do
  either (throwIO . InvalidOptionException) pure $ validateRunOptions opts
  let RunOptions{verbosity, monitoringPort, persistenceDir, offlineConfig} = opts
  env@Environment{party, otherParties, signingKey} <- initEnvironment opts
  withTracer verbosity $ \tracer' ->
    withMonitoring monitoringPort tracer' $ \tracer -> do
      traceWith tracer (NodeOptions opts)
      eq@EventQueue{putEvent} <- createEventQueue
      let RunOptions{hydraScriptsTxId, chainConfig, ledgerConfig} = opts
      protocolParams <- readJsonFileThrow protocolParametersFromJson (cardanoLedgerProtocolParametersFile ledgerConfig)
      pparams <- case toLedgerPParams ShelleyBasedEraBabbage protocolParams of
        Left err -> throwIO (ConfigurationException err)
        Right bpparams -> pure bpparams
      let onlineOrOfflineConfig = case offlineConfig of
            Nothing -> Right chainConfig
            Just offlineConfig' -> Left offlineConfig'

      let DirectChainConfig{networkId, nodeSocket} = chainConfig

      globals <- case offlineConfig of
        Nothing ->
          -- online
          newGlobals =<< queryGenesisParameters networkId nodeSocket QueryTip
        Just _ -> do
          -- offline
          systemStart <- SystemStart <$> getCurrentTime
          pure $ defaultGlobals{Ledger.systemStart = systemStart}

      withCardanoLedger pparams globals $ \ledger -> do
        persistence <- createStateChangePersistence (persistenceDir <> "/state") (offlineOptionsNormalizedUtxoWriteBackFilePath =<< leftToMaybe onlineOrOfflineConfig)
        (hs, chainStateHistory) <- loadState (contramap Node tracer) persistence initialChainState

        let headId = HeadId "HeadId"
        case offlineConfig of
          Nothing -> pure ()
          Just (OfflineConfig{initialUTxOFile}) -> do
            initialUTxO :: UTxOType Tx <- readJsonFileThrow (parseJSON @(UTxOType Tx)) initialUTxOFile
            initializeStateIfOffline chainStateHistory initialUTxO headId party (putEvent . OnChainEvent)

        checkHeadState (contramap Node tracer) env hs
        nodeState <- createNodeState hs
        -- Chain
        ctx <- case onlineOrOfflineConfig of
          Left _ -> pure (error "error: shouldnt be forced") -- this is only used in draftCommitTx in mkFakeL1Chain, which should be unused, so we can probably get rid of this in offline mode
          Right _ -> loadChainContext chainConfig party hydraScriptsTxId
        withChain onlineOrOfflineConfig tracer globals ctx signingKey chainStateHistory headId (putEvent . OnChainEvent) $ \chain -> do
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

  withChain onlineOrOfflineConfig tracer globals ctx signingKey chainStateHistory headId putEvent cont = case onlineOrOfflineConfig of
    Left offlineConfig -> withOfflineChain (contramap DirectChain tracer) offlineConfig globals ctx headId chainStateHistory (putEvent . OnChainEvent) cont
    Right onlineConfig -> do
      wallet <- mkTinyWallet (contramap DirectChain tracer) onlineConfig
      withDirectChain (contramap DirectChain tracer) onlineConfig ctx wallet chainStateHistory (putEvent . OnChainEvent) cont

  withCardanoLedger protocolParams globals action =
    let ledgerEnv = newLedgerEnv protocolParams
     in action (Ledger.cardanoLedger globals ledgerEnv)

  withCardanoLedgerOffline OfflineConfig{} protocolParams globals action = do
    -- TODO(Elaine): double check previous messy branch for any other places where we query node
    -- TODO(Elaine): instead of reading file, we can embed our own defaults with shelleyGenesisDefaults
    -- that would be more convenient, but offer less control
    -- NOTE(Elaine): we need globals here to call Cardano.Ledger.Shelley.API.Mempool.applyTxs ultimately
    -- that function could probably take less info but it's upstream of hydra itself i believe
    let ledgerEnv = newLedgerEnv protocolParams
    action (Ledger.cardanoLedger globals ledgerEnv)
  withCardanoLedgerOnline protocolParams globals action = do
    let ledgerEnv = newLedgerEnv protocolParams
    action (Ledger.cardanoLedger globals ledgerEnv)

identifyNode :: RunOptions -> RunOptions
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt

-- TODO(ELAINE): figure out a less strange way to do this

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
