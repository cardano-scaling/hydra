{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude

import CardanoNode (
  EndToEndLog (..),
  findFileStartingAtDirectory,
  findRunningCardanoNode,
  waitForFullySynchronized,
  withCardanoNodeDevnet,
  withCardanoNodeOnKnownNetwork,
 )
import Hydra.Cardano.Api (TxId, serialiseToRawBytesHexText)
import Hydra.Chain.Backend (blockfrostProjectPath)
import Hydra.Cluster.Faucet qualified as Faucet
import Hydra.Cluster.Fixture (Actor (Faucet), KnownNetwork (..))
import Hydra.Cluster.Mithril (downloadLatestSnapshotTo)
import Hydra.Cluster.Options (Options (..), PublishOrReuse (Publish, Reuse), Scenario (..), UseMithril (UseMithril), parseOptions)
import Hydra.Cluster.Scenarios (respendNTimes, singlePartyHeadFullLifeCycle, singlePartyOpenAHead)
import Hydra.Cluster.Util (mkSmokeTiming, mkTestTiming)
import Hydra.Logging (Tracer, traceWith, withTracerOutputTo)
import Hydra.Options (BlockfrostOptions (..), ChainBackendOptions (..), defaultBlockfrostOptions)
import Options.Applicative (ParserInfo, execParser, fullDesc, header, helper, info, progDesc)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hydra.Prelude (withTempDir)

main :: IO ()
main =
  execParser hydraClusterOptions >>= run

run :: Options -> IO ()
run options =
  withTracerOutputTo NoBuffering stdout "hydra-cluster" $ \tracer -> do
    traceWith tracer ClusterOptions{options}
    let fromCardanoNode = contramap FromCardanoNode tracer
    let blockfrostNetworks = [BlockfrostPreview]
    withStateDirectory $ \workDir ->
      case knownNetwork of
        Just network -> do
          if network `notElem` blockfrostNetworks
            then withRunningCardanoNode tracer workDir network $ \_ opts -> do
              waitForFullySynchronized fromCardanoNode (Direct opts)
              resolveHydraScripts tracer workDir (isMainnet network) (Direct opts)
                >>= singlePartyHeadFullLifeCycle tracer workDir (smokeTiming network) (Direct opts)
            else do
              bfProjectPath <- findFileStartingAtDirectory 3 blockfrostProjectPath
              let opts = Blockfrost defaultBlockfrostOptions{projectPath = bfProjectPath}
              resolveHydraScripts tracer workDir (isMainnet network) opts
                >>= singlePartyHeadFullLifeCycle tracer workDir (smokeTiming network) opts
        Nothing -> do
          withCardanoNodeDevnet fromCardanoNode workDir $ \_ opts -> do
            txId <- resolveHydraScripts tracer workDir False (Direct opts)
            let hydraScriptsTxId = intercalate "," $ toString . serialiseToRawBytesHexText <$> txId
            let envPath = workDir </> ".env"
            writeFile envPath $ "HYDRA_SCRIPTS_TX_ID=" <> hydraScriptsTxId
            singlePartyOpenAHead tracer workDir (Direct opts) txId persistenceRotateAfter $ \client walletSk _headId -> do
              case scenario of
                Idle -> forever $ pure ()
                RespendUTxO -> do
                  -- Start respending the same UTxO with a 100ms delay.
                  forever $ respendNTimes client walletSk 0.1 100
 where
  Options{knownNetwork, stateDirectory, publishHydraScripts, useMithril, scenario, persistenceRotateAfter} = options

  withRunningCardanoNode tracer workDir network action =
    findRunningCardanoNode (contramap FromCardanoNode tracer) workDir network >>= \case
      Just (blockTime, opts) ->
        action blockTime opts
      Nothing -> do
        when (useMithril == UseMithril) $ do
          let dbDir = workDir </> "db"
          let networkFile = workDir </> ".mithril-network"
          dbExists <- doesDirectoryExist dbDir
          storedNetwork <- (decodeUtf8 @Text <$> readFileBS networkFile) `catch` (\(_ :: SomeException) -> pure "")
          when (not dbExists || storedNetwork /= show network) $ do
            removeDirectoryRecursive dbDir `catch` (\(_ :: SomeException) -> pure ())
            downloadLatestSnapshotTo (contramap FromMithril tracer) network workDir
            writeFileBS networkFile (encodeUtf8 @Text $ show network)
        withCardanoNodeOnKnownNetwork (contramap FromCardanoNode tracer) workDir network action

  withStateDirectory action = case stateDirectory of
    Nothing -> withTempDir ("hydra-cluster-" <> show knownNetwork) action
    Just sd -> action sd

  -- The testnet smoke runs are dominated by protocol waits, so shorten those.
  -- Mainnet keeps the end-to-end timings: it runs once per release, so there is
  -- nothing to gain there, and a shorter contestation period would only narrow
  -- the window its close transaction has to be included -- on a head holding
  -- real funds, with no resubmit on expiry.
  smokeTiming network
    | isMainnet network = mkTestTiming
    | otherwise = mkSmokeTiming

  -- NOTE: On testnets 'Publish' does not mean "publish unconditionally":
  -- scripts already published from the same work directory are validated
  -- against the chain by 'Faucet.publishOrReuseHydraScripts' and reused, which
  -- saves a transaction and its confirmation; a script change invalidates them.
  -- Mainnet always publishes: that validation cannot tell a changed script from
  -- a transient query failure, and guessing wrong there spends real funds.
  resolveHydraScripts :: Tracer IO EndToEndLog -> FilePath -> Bool -> ChainBackendOptions -> IO [TxId]
  resolveHydraScripts tracer workDir mainnet opts =
    case publishHydraScripts of
      Publish -> do
        hydraScriptsTxId <-
          if mainnet
            then Faucet.publishHydraScriptsAs opts Faucet
            else Faucet.publishOrReuseHydraScripts opts Faucet workDir
        traceWith tracer $ PublishedHydraScriptsAt{hydraScriptsTxId}
        pure hydraScriptsTxId
      Reuse hydraScriptsTxId -> do
        traceWith tracer $ UsingHydraScriptsAt{hydraScriptsTxId}
        pure hydraScriptsTxId

  -- NOTE: Matching the testnets rather than the mainnets, so that a network
  -- added to 'KnownNetwork' later defaults to the careful side here.
  isMainnet = \case
    Preview -> False
    Preproduction -> False
    BlockfrostPreview -> False
    BlockfrostPreprod -> False
    Mainnet -> True
    BlockfrostMainnet -> True

hydraClusterOptions :: ParserInfo Options
hydraClusterOptions =
  info
    (parseOptions <**> helper)
    ( fullDesc
        <> progDesc
          "Starts a local cluster of interconnected Hydra nodes \
          \talking to a configurable cardano network.\n\
          \Right now, a cluster of size 1 is started and either walks a \
          \simple 1 party Hydra Head through its full life cycle, or\
          \just provides a running standalone cardano network."
        <> header "hydra-cluster - running hydra-node clusters"
    )
