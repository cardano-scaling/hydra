module Hydra.Node.Run where

import "hydra-prelude" Hydra.Prelude hiding (fromList)

import "cardano-ledger-core" Cardano.Ledger.BaseTypes (Globals (..), boundRational, mkActiveSlotCoeff, unNonZero)
import "cardano-ledger-shelley" Cardano.Ledger.Shelley.API (computeRandomnessStabilisationWindow, computeStabilityWindow)
import "cardano-slotting" Cardano.Slotting.EpochInfo (fixedEpochInfo)
import "cardano-slotting" Cardano.Slotting.Time (mkSlotLength)
import "filepath" System.FilePath ((</>))
import "hydra-cardano-api" Hydra.Cardano.Api (
  GenesisParameters (..),
  LedgerEra,
  PParams,
  ProtocolParametersConversionError,
  ShelleyEra,
  SystemStart (..),
  Tx,
  toShelleyNetwork,
 )
import "hydra-node" Hydra.API.Server (APIServerConfig (..), withAPIServer)
import "hydra-node" Hydra.API.ServerOutputFilter (serverOutputFilter)
import "hydra-node" Hydra.Chain (ChainComponent, ChainStateHistory, maximumNumberOfParties)
import "hydra-node" Hydra.Chain.Backend (ChainBackend (queryGenesisParameters))
import "hydra-node" Hydra.Chain.Blockfrost (BlockfrostBackend (..))
import "hydra-node" Hydra.Chain.Cardano (withCardanoChain)
import "hydra-node" Hydra.Chain.Direct (DirectBackend (..))
import "hydra-node" Hydra.Chain.Direct.State (initialChainState)
import "hydra-node" Hydra.Chain.Offline (loadGenesisFile, withOfflineChain)
import "hydra-node" Hydra.Events (EventSink)
import "hydra-node" Hydra.Events.FileBased (mkFileBasedEventStore)
import "hydra-node" Hydra.Events.Rotation (EventStore (..), RotationConfig (..), newRotatedEventStore)
import "hydra-node" Hydra.HeadLogic (aggregateNodeState)
import "hydra-node" Hydra.HeadLogic.StateEvent (StateEvent (StateEvent, stateChanged), mkCheckpoint)
import "hydra-node" Hydra.Ledger (Ledger)
import "hydra-node" Hydra.Logging (Tracer, traceWith, withTracer)
import "hydra-node" Hydra.Logging.Messages (HydraLog (..))
import "hydra-node" Hydra.Logging.Monitoring (withMonitoring)
import "hydra-node" Hydra.Node (
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
import "hydra-node" Hydra.Node.Environment (Environment (..))
import "hydra-node" Hydra.Node.Network (NetworkConfiguration (..), withNetwork)
import "hydra-node" Hydra.Node.State (NodeState (..), initNodeState)
import "hydra-node" Hydra.Options (
  CardanoChainConfig (..),
  ChainBackendOptions (..),
  ChainConfig (..),
  InvalidOptions (..),
  LedgerConfig (..),
  OfflineChainConfig (..),
  RunOptions (..),
  validateRunOptions,
 )
import "hydra-node" Hydra.Persistence (createPersistenceIncremental)
import "hydra-node" Hydra.Utils (readJsonFileThrow)
import "hydra-tx" Hydra.Chain.ChainState (IsChainState (..))
import "hydra-tx" Hydra.Ledger.Cardano (cardanoLedger, newLedgerEnv)

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
        eventStore@EventStore{eventSource} <-
          prepareEventStore
            =<< mkFileBasedEventStore stateFile
            =<< createPersistenceIncremental (contramap Persistence tracer) stateFile
        -- NOTE: Add any custom sinks here
        let eventSinks :: [EventSink (StateEvent Tx) IO] = []
        wetHydraNode <- hydrate (contramap Node tracer) env ledger initialChainState eventStore eventSinks
        -- Chain
        withChain <- prepareChainComponent tracer env chainConfig
        withChain (chainStateHistory wetHydraNode) (wireChainInput wetHydraNode) $ \chain -> do
          -- API
          let apiServerConfig = APIServerConfig{host = apiHost, port = apiPort, tlsCertPath, tlsKeyPath, apiTransactionTimeout}
          withAPIServer apiServerConfig env stateFile party eventSource (contramap APIServer tracer) initialChainState chain pparams serverOutputFilter (wireClientInput wetHydraNode) $ \(apiSink, server) -> do
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
                    }
            withNetwork
              (contramap Network tracer)
              networkConfiguration
              (wireNetworkInput wetHydraNode)
              $ \network -> do
                -- Main loop
                connect chain network server wetHydraNode
                  <&> addEventSink apiSink
                    >>= runHydraNode
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
    , apiTransactionTimeout
    } = opts

getGlobalsForChain :: ChainConfig -> IO Globals
getGlobalsForChain = \case
  Offline OfflineChainConfig{ledgerGenesisFile} ->
    loadGenesisFile ledgerGenesisFile
      >>= newGlobals
  Cardano CardanoChainConfig{chainBackendOptions} ->
    case chainBackendOptions of
      Direct directOptions -> queryGenesisParameters (DirectBackend directOptions)
      Blockfrost blockfrostOptions -> queryGenesisParameters (BlockfrostBackend blockfrostOptions)
      >>= newGlobals

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
  -- NOTE: uses fixed epoch info for our L2 ledger
  epochInfo = fixedEpochInfo protocolParamEpochLength slotLength
  slotLength = mkSlotLength protocolParamSlotLength
