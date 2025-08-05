module Main where

import Hydra.Prelude

import CardanoNode (findRunningCardanoNode, waitForFullySynchronized, withCardanoNodeDevnet, withCardanoNodeOnKnownNetwork)
import Hydra.Cluster.Faucet (publishHydraScriptsAs)
import Hydra.Cluster.Fixture (Actor (Faucet))
import Hydra.Cluster.Mithril (downloadLatestSnapshotTo)
import Hydra.Cluster.Options (Options (..), PublishOrReuse (Publish, Reuse), Scenario (..), UseMithril (UseMithril), parseOptions)
import Hydra.Cluster.Scenarios (EndToEndLog (..), respendUTxO, singlePartyHeadFullLifeCycle, singlePartyOpenAHead)
import Hydra.Logging (Verbosity (Verbose), traceWith, withTracer)
import Options.Applicative (ParserInfo, execParser, fullDesc, header, helper, info, progDesc)
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hydra.Prelude (withTempDir)

main :: IO ()
main =
  execParser hydraClusterOptions >>= run

run :: Options -> IO ()
run options =
  withTracer (Verbose "hydra-cluster") $ \tracer -> do
    traceWith tracer ClusterOptions{options}
    let fromCardanoNode = contramap FromCardanoNode tracer
    withStateDirectory $ \workDir ->
      case knownNetwork of
        Just network ->
          withRunningCardanoNode tracer workDir network $ \node -> do
            waitForFullySynchronized fromCardanoNode node
            publishOrReuseHydraScripts tracer node
              >>= singlePartyHeadFullLifeCycle tracer workDir node
        Nothing -> do
          withCardanoNodeDevnet fromCardanoNode workDir $ \node -> do
            txId <- publishOrReuseHydraScripts tracer node
            singlePartyOpenAHead tracer workDir node txId persistenceRotateAfter $ \client walletSk _headId -> do
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
      Just node ->
        action node
      Nothing -> do
        when (useMithril == UseMithril) $ do
          removeDirectoryRecursive $ workDir </> "db"
          downloadLatestSnapshotTo (contramap FromMithril tracer) network workDir
        withCardanoNodeOnKnownNetwork (contramap FromCardanoNode tracer) workDir network action

  withStateDirectory action = case stateDirectory of
    Nothing -> withTempDir ("hydra-cluster-" <> show knownNetwork) action
    Just sd -> action sd

  publishOrReuseHydraScripts tracer node =
    case publishHydraScripts of
      Publish -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
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
