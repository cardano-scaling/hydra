{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import "hydra-prelude" Hydra.Prelude

import "directory" System.Directory (removeDirectoryRecursive)
import "filepath" System.FilePath ((</>))
import "hydra-cardano-api" Hydra.Cardano.Api (TxId, serialiseToRawBytesHexText)
import "hydra-cluster" CardanoNode (
  findFileStartingAtDirectory,
  findRunningCardanoNode,
  waitForFullySynchronized,
  withCardanoNodeDevnet,
  withCardanoNodeOnKnownNetwork,
 )
import "hydra-cluster" Hydra.Cluster.Faucet (publishHydraScriptsAs)
import "hydra-cluster" Hydra.Cluster.Fixture (Actor (Faucet), KnownNetwork (..))
import "hydra-cluster" Hydra.Cluster.Mithril (downloadLatestSnapshotTo)
import "hydra-cluster" Hydra.Cluster.Options (Options (..), PublishOrReuse (Publish, Reuse), Scenario (..), UseMithril (UseMithril), parseOptions)
import "hydra-cluster" Hydra.Cluster.Scenarios (EndToEndLog (..), respendUTxO, singlePartyHeadFullLifeCycle, singlePartyOpenAHead)
import "hydra-node" Hydra.Chain.Backend (ChainBackend, blockfrostProjectPath)
import "hydra-node" Hydra.Chain.Blockfrost (BlockfrostBackend (..))
import "hydra-node" Hydra.Logging (Tracer, traceWith, withTracerOutputTo)
import "hydra-node" Hydra.Options (BlockfrostOptions (..), defaultBlockfrostOptions)
import "hydra-test-utils" Test.Hydra.Prelude (withTempDir)
import "optparse-applicative" Options.Applicative (ParserInfo, execParser, fullDesc, header, helper, info, progDesc)

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
            then withRunningCardanoNode tracer workDir network $ \_ backend -> do
              waitForFullySynchronized fromCardanoNode backend
              publishOrReuseHydraScripts tracer backend
                >>= singlePartyHeadFullLifeCycle tracer workDir backend
            else do
              bfProjectPath <- findFileStartingAtDirectory 3 blockfrostProjectPath
              let backend = BlockfrostBackend $ defaultBlockfrostOptions{projectPath = bfProjectPath}
              publishOrReuseHydraScripts tracer backend
                >>= singlePartyHeadFullLifeCycle tracer workDir backend
        Nothing -> do
          withCardanoNodeDevnet fromCardanoNode workDir $ \_ backend -> do
            txId <- publishOrReuseHydraScripts tracer backend
            let hydraScriptsTxId = intercalate "," $ toString . serialiseToRawBytesHexText <$> txId
            let envPath = workDir </> ".env"
            writeFile envPath $ "HYDRA_SCRIPTS_TX_ID=" <> hydraScriptsTxId
            singlePartyOpenAHead tracer workDir backend txId persistenceRotateAfter $ \client walletSk _headId -> do
              case scenario of
                Idle -> forever $ pure ()
                RespendUTxO -> do
                  -- Start respending the same UTxO with a 100ms delay.
                  -- XXX: Should make this configurable
                  respendUTxO client walletSk 0.1
 where
  Options{knownNetwork, stateDirectory, publishHydraScripts, useMithril, scenario, persistenceRotateAfter} = options

  withRunningCardanoNode tracer workDir network action =
    findRunningCardanoNode (contramap FromCardanoNode tracer) workDir network >>= \case
      Just (blockTime, backend) ->
        action blockTime backend
      Nothing -> do
        when (useMithril == UseMithril) $ do
          removeDirectoryRecursive (workDir </> "db") `catch` (\(_ :: SomeException) -> pure ())
          downloadLatestSnapshotTo (contramap FromMithril tracer) network workDir
        withCardanoNodeOnKnownNetwork (contramap FromCardanoNode tracer) workDir network action

  withStateDirectory action = case stateDirectory of
    Nothing -> withTempDir ("hydra-cluster-" <> show knownNetwork) action
    Just sd -> action sd

  publishOrReuseHydraScripts :: ChainBackend backend => Tracer IO EndToEndLog -> backend -> IO [TxId]
  publishOrReuseHydraScripts tracer backend =
    case publishHydraScripts of
      Publish -> do
        hydraScriptsTxId <- publishHydraScriptsAs backend Faucet
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
