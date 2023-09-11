{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude

import Crypto.Random (getRandomBytes)
import Hydra.API.Server (Server (Server, sendOutput), withAPIServer)
import Hydra.API.ServerOutput (ServerOutput (PeerConnected, PeerDisconnected))
import Hydra.Cardano.Api (
  File (..),
  Key (SigningKey),
  getVerificationKey,
  serialiseToRawBytesHex,
  writeFileTextEnvelope, ShelleyBasedEra (..), toLedgerPParams, ProtocolParametersConversionError,
 )
import Hydra.Chain.CardanoClient (QueryPoint (..), queryGenesisParameters)
import Hydra.Chain.Direct (loadChainContext, mkTinyWallet, withDirectChain)
import Hydra.Chain.Direct.ScriptRegistry (publishHydraScripts)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.Chain.Direct.Util (readKeyPair)
import Hydra.Crypto (HydraKey, generateSigningKey)
import Hydra.HeadLogic (
  Environment (..),
  Event (..),
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
import Hydra.Network.Authenticate (Authenticated (Authenticated), withAuthentication)
import Hydra.Network.Heartbeat (withHeartbeat)
import Hydra.Network.Message (Connectivity (..))
import Hydra.Network.Ouroboros (withOuroborosNetwork)
import Hydra.Node (
  HydraNode (..),
  checkHeadState,
  createNodeState,
  initEnvironment,
  loadState,
  runHydraNode,
 )
import Hydra.Node.EventQueue (EventQueue (..), createEventQueue)
import Hydra.Options (
  ChainConfig (..),
  Command (GenHydraKey, Publish, Run),
  GenerateKeyPair (GenerateKeyPair, outputFile),
  LedgerConfig (..),
  PublishOptions (..),
  RunOptions (..),
  explain,
  parseHydraCommand,
  validateRunOptions,
 )
import Hydra.Persistence (createPersistenceIncremental)
import System.FilePath ((<.>))

newtype ConfigurationParseException = ConfigurationParseException ProtocolParametersConversionError
  deriving Show

instance Exception ConfigurationParseException

main :: IO ()
main = do
  command <- parseHydraCommand
  case command of
    Run options -> do
      either (die . explain) pure $ validateRunOptions options
      run (identifyNode options)
    Publish options ->
      publish options
    GenHydraKey outputFile ->
      genHydraKeys outputFile
 where
  run opts = do
    let RunOptions{verbosity, monitoringPort, persistenceDir} = opts
    env@Environment{party, otherParties, signingKey} <- initEnvironment opts
    withTracer verbosity $ \tracer' ->
      withMonitoring monitoringPort tracer' $ \tracer -> do
        traceWith tracer (NodeOptions opts)
        eq@EventQueue{putEvent} <- createEventQueue
        let RunOptions{hydraScriptsTxId, chainConfig, ledgerConfig} = opts
        protocolParams <- readJsonFileThrow protocolParametersFromJson (cardanoLedgerProtocolParametersFile ledgerConfig)
        pparams <- case toLedgerPParams ShelleyBasedEraBabbage protocolParams of
          Left err -> throwIO (ConfigurationParseException err)
          Right bpparams -> pure bpparams
        withCardanoLedger chainConfig pparams $ \ledger -> do
          persistence <- createPersistenceIncremental $ persistenceDir <> "/state"
          (hs, chainStateHistory) <- loadState (contramap Node tracer) persistence initialChainState
          checkHeadState (contramap Node tracer) env hs
          nodeState <- createNodeState hs
          -- Chain
          ctx <- loadChainContext chainConfig party otherParties hydraScriptsTxId
          wallet <- mkTinyWallet (contramap DirectChain tracer) chainConfig
          withDirectChain (contramap DirectChain tracer) chainConfig ctx wallet chainStateHistory (putEvent . OnChainEvent) $ \chain -> do
            -- API
            let RunOptions{host, port, peers, nodeId} = opts
                putNetworkEvent (Authenticated msg otherParty) = putEvent $ NetworkEvent defaultTTL otherParty msg
                RunOptions{apiHost, apiPort} = opts
            apiPersistence <- createPersistenceIncremental $ persistenceDir <> "/server-output"
            withAPIServer apiHost apiPort party apiPersistence (contramap APIServer tracer) chain pparams (putEvent . ClientEvent) $ \server -> do
              -- Network
              withNetwork tracer server signingKey otherParties host port peers nodeId putNetworkEvent $ \hn -> do
                -- Main loop
                runHydraNode (contramap Node tracer) $
                  HydraNode
                    { eq
                    , hn = contramap (`Authenticated` party) hn
                    , nodeState
                    , oc = chain
                    , server
                    , ledger
                    , env
                    , persistence
                    }

  publish opts = do
    (_, sk) <- readKeyPair (publishSigningKey opts)
    let PublishOptions{publishNetworkId = networkId, publishNodeSocket} = opts
    txId <- publishHydraScripts networkId publishNodeSocket sk
    putStr (decodeUtf8 (serialiseToRawBytesHex txId))

  genHydraKeys GenerateKeyPair{outputFile} = do
    sk :: SigningKey HydraKey <- generateSigningKey <$> getRandomBytes 16
    void $ writeFileTextEnvelope (File (outputFile <.> "sk")) Nothing sk
    void $ writeFileTextEnvelope (File (outputFile <.> "vk")) Nothing (getVerificationKey sk)

  withNetwork tracer Server{sendOutput} signingKey parties host port peers nodeId =
    let localhost = Host{hostname = show host, port}
        connectionMessages = \case
          Connected nodeid -> sendOutput $ PeerConnected nodeid
          Disconnected nodeid -> sendOutput $ PeerDisconnected nodeid
     in withAuthentication (contramap Authentication tracer) signingKey parties $ withHeartbeat nodeId connectionMessages $ withOuroborosNetwork (contramap Network tracer) localhost peers

  withCardanoLedger chainConfig protocolParams action = do
    let DirectChainConfig{networkId, nodeSocket} = chainConfig
    globals <- newGlobals =<< queryGenesisParameters networkId nodeSocket QueryTip
    let ledgerEnv = newLedgerEnv protocolParams
    action (Ledger.cardanoLedger globals ledgerEnv)

identifyNode :: RunOptions -> RunOptions
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
