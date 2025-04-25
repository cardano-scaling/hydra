module Hydra.Node.Run where

import Hydra.Prelude hiding (fromList)

import Cardano.Ledger.BaseTypes (Globals (..), boundRational, mkActiveSlotCoeff, unNonZero)
import Cardano.Ledger.Shelley.API (computeRandomnessStabilisationWindow, computeStabilityWindow)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (mkSlotLength)
import Hydra.API.Server (APIServerConfig (..), withAPIServer)
import Hydra.API.ServerOutputFilter (serverOutputFilter)
import Hydra.Cardano.Api (
  GenesisParameters (..),
  ProtocolParametersConversionError,
  ShelleyEra,
  SystemStart (..),
  toShelleyNetwork,
 )
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Chain.CardanoClient (QueryPoint (..), queryGenesisParameters)
import Hydra.Chain.Direct (loadChainContext, mkTinyWallet, withDirectChain)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.Chain.Offline (loadGenesisFile, withOfflineChain)
import Hydra.Events.SqlLiteBased (eventPairFromPersistenceIncremental)
import Hydra.Ledger.Cardano (cardanoLedger, newLedgerEnv)
import Hydra.Logging (traceWith, withTracer)
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
import Hydra.Node.Network (NetworkConfiguration (..), withNetwork)
import Hydra.Options (
  ChainConfig (..),
  DirectChainConfig (..),
  InvalidOptions (..),
  LedgerConfig (..),
  OfflineChainConfig (..),
  RunOptions (..),
  validateRunOptions,
 )
import Hydra.SqlLitePersistence (createPersistenceIncremental)
import Hydra.Tx.Environment (Environment (..))
import Hydra.Utils (readJsonFileThrow)

data ConfigurationException
  = -- XXX: this is not used
    ConfigurationException ProtocolParametersConversionError
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
  withTracer verbosity $ \tracer' -> do
    traceWith tracer' (NodeOptions opts)
    withMonitoring monitoringPort tracer' $ \tracer -> do
      env@Environment{party, otherParties, signingKey} <- initEnvironment opts
      -- Ledger
      pparams <- readJsonFileThrow parseJSON (cardanoLedgerProtocolParametersFile ledgerConfig)
      globals <- getGlobalsForChain chainConfig
      withCardanoLedger pparams globals $ \ledger -> do
        incPersistence <- createPersistenceIncremental (persistenceDir <> "/state")
        -- Hydrate with event source and sinks
        (eventSource, filePersistenceSink) <- eventPairFromPersistenceIncremental incPersistence
        -- NOTE: Add any custom sink setup code here
        -- customSink <- createCustomSink
        let eventSinks =
              [ filePersistenceSink
              -- NOTE: Add any custom sinks here
              -- , customSink
              ]
        wetHydraNode <- hydrate (contramap Node tracer) env ledger initialChainState eventSource eventSinks
        -- Chain
        withChain <- prepareChainComponent tracer env chainConfig
        withChain (chainStateHistory wetHydraNode) (wireChainInput wetHydraNode) $ \chain -> do
          -- API
          let apiServerConfig = APIServerConfig{host = apiHost, port = apiPort, tlsCertPath, tlsKeyPath}
          withAPIServer apiServerConfig env party eventSource (contramap APIServer tracer) chain pparams serverOutputFilter (wireClientInput wetHydraNode) $ \(apiSink, server) -> do
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
  addEventSink sink node = node{eventSinks = sink : eventSinks node}

  withCardanoLedger protocolParams globals action =
    let ledgerEnv = newLedgerEnv protocolParams
     in action (cardanoLedger globals ledgerEnv)

  prepareChainComponent tracer Environment{party, otherParties} = \case
    Offline cfg ->
      pure $ withOfflineChain cfg party otherParties
    Direct cfg -> do
      ctx <- loadChainContext cfg party
      wallet <- mkTinyWallet (contramap DirectChain tracer) cfg
      pure $ withDirectChain (contramap DirectChain tracer) cfg ctx wallet

  RunOptions
    { verbosity
    , monitoringPort
    , persistenceDir
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
    } = opts

getGlobalsForChain :: ChainConfig -> IO Globals
getGlobalsForChain = \case
  Offline OfflineChainConfig{ledgerGenesisFile} ->
    loadGenesisFile ledgerGenesisFile
      >>= newGlobals
  Direct DirectChainConfig{networkId, nodeSocket} ->
    queryGenesisParameters networkId nodeSocket QueryTip
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
