module Hydra.Node.Run where

import Hydra.Prelude hiding (fromList)

import Hydra.API.Server (Server (..), withAPIServer)
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Cardano.Api (
  ProtocolParametersConversionError,
 )
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Chain.CardanoClient (QueryPoint (..), queryGenesisParameters)
import Hydra.Chain.Direct (loadChainContext, mkTinyWallet, withDirectChain)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.Chain.Offline (loadGenesisFile, withOfflineChain)
import Hydra.HeadLogic (
  Environment (..),
  Input (..),
  defaultTTL,
 )
import Hydra.Ledger.Cardano qualified as Ledger
import Hydra.Ledger.Cardano.Configuration (
  Globals,
  newGlobals,
  newLedgerEnv,
  pparamsFromJson,
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
  loadStateEventSource,
  runHydraNode,
 )
import Hydra.Node.InputQueue (InputQueue (..), createInputQueue)
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
import Hydra.Persistence (NewPersistenceIncremental (..), createNewPersistenceIncremental, createPersistenceIncremental, eventPairFromPersistenceIncremental)

import Data.List.NonEmpty qualified as NE

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
      inputQueue@InputQueue{enqueue} <- createInputQueue
      let RunOptions{chainConfig, ledgerConfig} = opts
      pparams <- readJsonFileThrow pparamsFromJson (cardanoLedgerProtocolParametersFile ledgerConfig)

      globals <- getGlobalsForChain chainConfig

      withCardanoLedger pparams globals $ \ledger -> do
        -- persistence <- createPersistenceIncremental $ persistenceDir <> "/state"
        -- TODO(Elaine): remove in favor of eventSource/Sink directly
        -- (eventSource, eventSink) <- createEventPairIncremental $ persistenceDir <> "/state"

        -- let -- (eventSource, eventSink) = eventPairFromPersistenceIncremental persistence
        --     eventSinks = eventSink :| [] --FIXME(Elaine): load other event sinks
        --     eventSinksSansSource = [] --TODO(Elaine): this needs a better name. essentially, don't load events back into where they came from, at least until disk-based persistence can handle redelivery
        let eventSinksSansSource = undefined
        persistence@NewPersistenceIncremental{eventSource, eventSinks} <- createNewPersistenceIncremental $ persistenceDir <> "/state"

        (hs, chainStateHistory) <- loadStateEventSource (contramap Node tracer) eventSource (NE.toList eventSinks) initialChainState

        checkHeadState (contramap Node tracer) env hs
        nodeState <- createNodeState hs
        -- Chain
        withChain <- prepareChainComponent tracer env chainConfig
        withChain chainStateHistory (enqueue . ChainInput) $ \chain -> do
          -- API
          let RunOptions{host, port, peers, nodeId} = opts
              putNetworkEvent (Authenticated msg otherParty) = enqueue $ NetworkInput defaultTTL otherParty msg
              RunOptions{apiHost, apiPort} = opts
          apiPersistence <- createNewPersistenceIncremental $ persistenceDir <> "/server-output"
          withAPIServer apiHost apiPort party apiPersistence (contramap APIServer tracer) chain pparams (enqueue . ClientInput) $ \server -> do
            -- Network
            let networkConfiguration = NetworkConfiguration{persistenceDir, signingKey, otherParties, host, port, peers, nodeId}
            withNetwork tracer (connectionMessages server) networkConfiguration putNetworkEvent $ \hn -> do
              -- Main loop
              runHydraNode (contramap Node tracer) $
                HydraNode
                  { inputQueue
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

  prepareChainComponent tracer Environment{party} = \case
    Offline cfg ->
      pure $ withOfflineChain cfg party
    Direct cfg -> do
      ctx <- loadChainContext cfg party
      wallet <- mkTinyWallet (contramap DirectChain tracer) cfg
      pure $ withDirectChain (contramap DirectChain tracer) cfg ctx wallet

getGlobalsForChain :: ChainConfig -> IO Globals
getGlobalsForChain = \case
  Offline OfflineChainConfig{ledgerGenesisFile} ->
    loadGenesisFile ledgerGenesisFile
      >>= newGlobals
  Direct DirectChainConfig{networkId, nodeSocket} ->
    queryGenesisParameters networkId nodeSocket QueryTip
      >>= newGlobals

identifyNode :: RunOptions -> RunOptions
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
