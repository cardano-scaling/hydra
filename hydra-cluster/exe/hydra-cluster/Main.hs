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
import Hydra.Cluster.Faucet (publishHydraScriptsAs)
import Hydra.Cluster.Fixture (Actor (Faucet), KnownNetwork (..))
import Hydra.Cluster.Mithril (downloadLatestSnapshotTo)
import Hydra.Cluster.Options (Options (..), PublishOrReuse (Publish, Reuse), Scenario (..), UseMithril (UseMithril), parseOptions)
import Hydra.Cluster.Scenarios (respendNTimes, singlePartyHeadFullLifeCycle, singlePartyOpenAHead)
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
              publishOrReuseHydraScripts tracer (Direct opts)
                >>= singlePartyHeadFullLifeCycle tracer workDir (Direct opts)
            else do
              bfProjectPath <- findFileStartingAtDirectory 3 blockfrostProjectPath
              let opts = Blockfrost defaultBlockfrostOptions{projectPath = bfProjectPath}
              publishOrReuseHydraScripts tracer opts
                >>= singlePartyHeadFullLifeCycle tracer workDir opts
        Nothing -> do
          withCardanoNodeDevnet fromCardanoNode workDir $ \_ opts -> do
            txId <- publishOrReuseHydraScripts tracer (Direct opts)
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

  publishOrReuseHydraScripts :: Tracer IO EndToEndLog -> ChainBackendOptions -> IO [TxId]
  publishOrReuseHydraScripts tracer opts =
    case publishHydraScripts of
      Publish -> do
        hydraScriptsTxId <- publishHydraScriptsAs opts Faucet
        traceWith tracer $ PublishedHydraScriptsAt{hydraScriptsTxId}
        pure hydraScriptsTxId
      Reuse hydraScriptsTxId -> do
        traceWith tracer $ UsingHydraScriptsAt{hydraScriptsTxId}
        pure hydraScriptsTxId

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
