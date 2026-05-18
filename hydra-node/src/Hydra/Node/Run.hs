module Hydra.Node.Run where

import Hydra.Prelude hiding (fromList)

import Cardano.Ledger.BaseTypes (Globals (..), boundRational, mkActiveSlotCoeff, unNonZero)
import Cardano.Ledger.Shelley.API (computeRandomnessStabilisationWindow, computeStabilityWindow)
import Cardano.Slotting.EpochInfo (fixedEpochInfo, hoistEpochInfo)
import Cardano.Slotting.Time (mkSlotLength)
import Control.Concurrent.Class.MonadSTM (newTVarIO, readTVar, writeTVar)
import Control.Monad.Trans.Except (runExcept)
import Hydra.API.Server (APIServerConfig (..), withAPIServer)
import Hydra.API.ServerOutputFilter (serverOutputFilter)
import Hydra.Cardano.Api (EraHistory (EraHistory), GenesisParameters (..), LedgerEra, PParams, ProtocolParametersConversionError, ShelleyEra, SystemStart (..), Tx, toShelleyNetwork)
import Hydra.Chain (ChainComponent, ChainStateHistory, maximumNumberOfParties)
import Hydra.Chain.Backend (ChainBackend (queryEraHistory, queryGenesisParameters))
import Hydra.Chain.Blockfrost (runBlockfrostBackend)
import Hydra.Chain.Cardano (withCardanoChain)
import Hydra.Chain.CardanoClient (QueryPoint (..))
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.Chain.Direct (runDirectBackend)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.Chain.Offline (loadGenesisFile, withOfflineChain)
import Hydra.Events (EventSink, mkEventSink)
import Hydra.Events.Rotation (EventStore (..), RotationConfig (..), newRotatedEventStore)
import Hydra.Events.SQLiteBased (withSQLiteEventStore)
import Hydra.HeadLogic (aggregateNodeState)
import Hydra.HeadLogic.Outcome qualified as StateChanged
import Hydra.HeadLogic.StateEvent (StateEvent (StateEvent, stateChanged), mkCheckpoint)
import Hydra.Ledger (Ledger)
import Hydra.Ledger.Cardano (cardanoLedger, newLedgerEnv)
import Hydra.Logging (Tracer, traceWith, withTracer)
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Logging.Monitoring (withMonitoring)
import Hydra.Node (
  HydraNode (eventSinks),
  chainStateHistory,
  connect,
  hydrate,
  initEnvironment,
  runHydraNode,
  wireChainInput,
  wireClientInput,
  wireNetworkInput,
 )
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.Network (NetworkConfiguration (..), withNetwork)
import Hydra.Node.State (NodeState (..), initNodeState)
import Hydra.Options (
  CardanoChainConfig (..),
  ChainBackendOptions (..),
  ChainConfig (..),
  InvalidOptions (..),
  LedgerConfig (..),
  OfflineChainConfig (..),
  RunOptions (..),
  validateRunOptions,
 )
import Hydra.Tx.KZGTrustedSetup qualified as KZG
import Hydra.Utils (readJsonFileThrow)
import Ouroboros.Consensus.HardFork.History qualified as Consensus
import System.FilePath ((</>))

data ConfigurationException
  = -- XXX: this is not used
    ConfigurationException ProtocolParametersConversionError
  | InvalidOptionException InvalidOptions
  deriving stock (Show)

instance Exception ConfigurationException where
  displayException = \case
    InvalidOptionException MaximumNumberOfPartiesExceeded ->
      "Maximum number of parties is currently set to: " <> show maximumNumberOfParties
    InvalidOptionException CardanoAndHydraKeysMismatch ->
      "Number of loaded cardano and hydra keys needs to match"
    ConfigurationException err ->
      "Incorrect protocol parameters configuration provided: " <> show err

run :: RunOptions -> IO ()
run opts = do
  -- Force all KZG G1 points into memory before opening any sockets.
  -- initTx (and snapshot signing) call getAccumulatorHash, which on first
  -- access parses ~300KB of JSON and BLS-decompresses 4096 curve points.
  -- Doing it here means the API server only starts after the warm-up, so
  -- clients cannot connect and time out waiting for HeadIsOpen while the
  -- node is still initialising the cryptographic setup.
  void $ evaluate $ either (error . show) length KZG.g1BuiltinPoints
  either (throwIO . InvalidOptionException) pure $ validateRunOptions opts
  withTracer verbosity $ \tracer' -> do
    traceWith tracer' (NodeOptions opts)
    withMonitoring monitoringPort tracer' $ \tracer -> do
      env@Environment{party, otherParties, signingKey} <- initEnvironment opts
      -- Ledger
      pparams <- readJsonFileThrow parseJSON (cardanoLedgerProtocolParametersFile ledgerConfig)
      globals <- getGlobalsForChain chainConfig
      withCardanoLedger pparams globals $ \ledger -> do
        -- Hydrate with event source and sinks
        let stateFile = persistenceDir </> "state"
            dbFile = persistenceDir </> "hydra.db"
        withSQLiteEventStore (contramap SQLite tracer') dbFile stateFile $ \store -> do
          eventStore@EventStore{eventSource} <- prepareEventStore store
          -- NOTE: Add any custom sinks here
          let eventSinks :: [EventSink (StateEvent Tx) IO] = []
          wetHydraNode <- hydrate (contramap Node tracer) env ledger initialChainState eventStore eventSinks
          traceWith tracer' NodeHydrated
          -- Chain
          withChain <- prepareChainComponent tracer env chainConfig
          withChain (chainStateHistory wetHydraNode) (wireChainInput wetHydraNode) $ \chain -> do
            traceWith tracer' ChainBackendStarted
            -- API
            let apiServerConfig = APIServerConfig{host = apiHost, port = apiPort, tlsCertPath, tlsKeyPath, apiTransactionTimeout}
            withAPIServer apiServerConfig env party eventSource (contramap APIServer tracer) initialChainState chain pparams serverOutputFilter (wireClientInput wetHydraNode) $ \(apiSink, server) -> do
              -- Network
              let networkConfiguration =
                    NetworkConfiguration
                      { persistenceDir
                      , signingKey
                      , otherParties
                      , listen
                      , advertise = fromMaybe listen advertise
                      , peers
                      , nodeId
                      , whichEtcd
                      , joinExistingCluster
                      }
              -- The accepted-parties set and the speculatively-accepted
              -- joining party are dynamic for the dynamic-head-participants
              -- feature (issue #1813). We start from the static
              -- 'Environment.otherParties' and a 'Nothing' joining party,
              -- and update both via an event sink that watches
              -- 'JoinRecorded' / 'ParametersChanged' / 'LeaveRecorded'
              -- state events. Each inbound network message reads a fresh
              -- snapshot of these 'TVar's in 'withAuthentication'.
              livePartiesVar <- newTVarIO otherParties
              joiningPartyVar <- newTVarIO Nothing
              let dynamicPartiesSink :: EventSink (StateEvent Tx) IO
                  dynamicPartiesSink = mkEventSink $ \StateEvent{stateChanged} ->
                    atomically $ case stateChanged of
                      StateChanged.JoinRecorded{joiningParty} ->
                        writeTVar joiningPartyVar (Just joiningParty)
                      StateChanged.ParametersChanged{newParties} -> do
                        writeTVar livePartiesVar (filter (/= party) newParties)
                        writeTVar joiningPartyVar Nothing
                      StateChanged.LeaveRecorded{} ->
                        -- Leaving is finalized via 'ParametersChanged'; nothing
                        -- to do at the auth layer yet.
                        pure ()
                      _ -> pure ()
              withNetwork
                (contramap Network tracer)
                networkConfiguration
                (readTVar livePartiesVar)
                (readTVar joiningPartyVar)
                (wireNetworkInput wetHydraNode)
                $ \network -> do
                  traceWith tracer' NetworkStarted
                  -- Main loop
                  node <-
                    connect chain network server wetHydraNode
                      <&> addEventSink dynamicPartiesSink . addEventSink apiSink
                  traceWith tracer' EnteringMainloop
                  runHydraNode node
 where
  addEventSink :: EventSink (StateEvent tx) m -> HydraNode tx m -> HydraNode tx m
  addEventSink sink node = node{eventSinks = sink : eventSinks node}

  withCardanoLedger :: PParams LedgerEra -> Globals -> (Ledger Tx -> m a) -> m a
  withCardanoLedger protocolParams globals action =
    let ledgerEnv = newLedgerEnv protocolParams
     in action (cardanoLedger globals ledgerEnv)

  prepareChainComponent ::
    Monad m =>
    Tracer IO (HydraLog tx) ->
    Environment ->
    ChainConfig ->
    m (ChainStateHistory Tx -> ChainComponent Tx IO a)
  prepareChainComponent tracer Environment{party, otherParties} = \case
    Offline cfg -> pure $ withOfflineChain cfg party otherParties
    Cardano cfg -> pure $ withCardanoChain (contramap DirectChain tracer) cfg party

  prepareEventStore eventStore = do
    case RotateAfter <$> persistenceRotateAfter of
      Nothing ->
        pure eventStore
      Just rotationConfig -> do
        let initialState = initNodeState initialChainState
        let aggregator :: IsChainState tx => NodeState tx -> StateEvent tx -> NodeState tx
            aggregator s StateEvent{stateChanged} = aggregateNodeState s stateChanged
        newRotatedEventStore rotationConfig initialState aggregator mkCheckpoint eventStore

  RunOptions
    { verbosity
    , monitoringPort
    , persistenceDir
    , persistenceRotateAfter
    , chainConfig
    , ledgerConfig
    , listen
    , advertise
    , peers
    , nodeId
    , apiHost
    , apiPort
    , tlsCertPath
    , tlsKeyPath
    , whichEtcd
    , joinExistingCluster
    , apiTransactionTimeout
    } = opts

getGlobalsForChain :: ChainConfig -> IO Globals
getGlobalsForChain = \case
  Offline OfflineChainConfig{ledgerGenesisFile} ->
    -- Offline/devnet: single era, fixedEpochInfo is correct
    loadGenesisFile ledgerGenesisFile
      >>= newGlobals
  Cardano CardanoChainConfig{chainBackendOptions} ->
    -- Online mode: query era history from the chain for correct
    -- slot-to-time conversions in Plutus script evaluation.
    case chainBackendOptions of
      Direct directOptions -> runDirectBackend directOptions globalsFromBackend
      Blockfrost blockfrostOptions -> runBlockfrostBackend blockfrostOptions globalsFromBackend
 where
  globalsFromBackend :: (ChainBackend m, MonadThrow m) => m Globals
  globalsFromBackend = do
    genesis <- queryGenesisParameters
    eraHistory <- queryEraHistory QueryTip
    newGlobalsWithEraHistory genesis eraHistory

data GlobalsTranslationException = GlobalsTranslationException
  deriving stock (Eq, Show)

instance Exception GlobalsTranslationException

-- | Create new L2 ledger 'Globals' from 'GenesisParameters'.
--
-- Throws at least 'GlobalsTranslationException'
newGlobals :: MonadThrow m => GenesisParameters ShelleyEra -> m Globals
newGlobals genesisParameters = do
  case mkActiveSlotCoeff <$> boundRational protocolParamActiveSlotsCoefficient of
    Nothing -> throwIO GlobalsTranslationException
    Just slotCoeff -> do
      let k = unNonZero protocolParamSecurity
      pure $
        Globals
          { activeSlotCoeff = slotCoeff
          , epochInfo
          , maxKESEvo = fromIntegral protocolParamMaxKESEvolutions
          , maxLovelaceSupply = fromIntegral protocolParamMaxLovelaceSupply
          , networkId = toShelleyNetwork protocolParamNetworkId
          , quorum = fromIntegral protocolParamUpdateQuorum
          , randomnessStabilisationWindow = computeRandomnessStabilisationWindow k slotCoeff
          , securityParameter = protocolParamSecurity
          , slotsPerKESPeriod = fromIntegral protocolParamSlotsPerKESPeriod
          , stabilityWindow = computeStabilityWindow k slotCoeff
          , systemStart = SystemStart protocolParamSystemStart
          }
 where
  GenesisParameters
    { protocolParamSlotsPerKESPeriod
    , protocolParamUpdateQuorum
    , protocolParamMaxLovelaceSupply
    , protocolParamSecurity
    , protocolParamActiveSlotsCoefficient
    , protocolParamSystemStart
    , protocolParamNetworkId
    , protocolParamMaxKESEvolutions
    , protocolParamEpochLength
    , protocolParamSlotLength
    } = genesisParameters
  -- NOTE: uses fixed epoch info for our L2 ledger (only correct for devnet/offline)
  epochInfo = fixedEpochInfo protocolParamEpochLength slotLength
  slotLength = mkSlotLength protocolParamSlotLength

-- | Create new L2 ledger 'Globals' using a proper 'EraHistory' for era-aware
-- slot-to-time conversions. This ensures Plutus scripts receive correct
-- POSIXTime values in their ScriptContext on multi-era chains (mainnet/testnet).
--
-- Throws at least 'GlobalsTranslationException'
newGlobalsWithEraHistory :: MonadThrow m => GenesisParameters ShelleyEra -> EraHistory -> m Globals
newGlobalsWithEraHistory genesisParameters (EraHistory interpreter) = do
  case mkActiveSlotCoeff <$> boundRational protocolParamActiveSlotsCoefficient of
    Nothing -> throwIO GlobalsTranslationException
    Just slotCoeff -> do
      let k = unNonZero protocolParamSecurity
      pure $
        Globals
          { activeSlotCoeff = slotCoeff
          , epochInfo = eraAwareEpochInfo
          , maxKESEvo = fromIntegral protocolParamMaxKESEvolutions
          , maxLovelaceSupply = fromIntegral protocolParamMaxLovelaceSupply
          , networkId = toShelleyNetwork protocolParamNetworkId
          , quorum = fromIntegral protocolParamUpdateQuorum
          , randomnessStabilisationWindow = computeRandomnessStabilisationWindow k slotCoeff
          , securityParameter = protocolParamSecurity
          , slotsPerKESPeriod = fromIntegral protocolParamSlotsPerKESPeriod
          , stabilityWindow = computeStabilityWindow k slotCoeff
          , systemStart = SystemStart protocolParamSystemStart
          }
 where
  GenesisParameters
    { protocolParamSlotsPerKESPeriod
    , protocolParamUpdateQuorum
    , protocolParamMaxLovelaceSupply
    , protocolParamSecurity
    , protocolParamActiveSlotsCoefficient
    , protocolParamSystemStart
    , protocolParamNetworkId
    , protocolParamMaxKESEvolutions
    } = genesisParameters
  eraAwareEpochInfo =
    hoistEpochInfo (first show . runExcept) $
      Consensus.interpreterToEpochInfo interpreter
