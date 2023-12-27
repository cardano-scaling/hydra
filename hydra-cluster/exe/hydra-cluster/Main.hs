module Main where

import Hydra.Prelude

import CardanoNode (withCardanoNodeDevnet, withCardanoNodeOnKnownNetwork)
import Hydra.Cluster.Faucet (publishHydraScriptsAs)
import Hydra.Cluster.Fixture (Actor (Faucet))
import Hydra.Cluster.Options (Options (..), PublishOrReuse (Publish, Reuse), parseOptions)
import Hydra.Cluster.Scenarios (EndToEndLog (..), singlePartyHeadFullLifeCycle, singlePartyOpenAHead)
import Hydra.Logging (Verbosity (Verbose), traceWith, withTracer)
import HydraNode (HydraClient (..))
import Options.Applicative (ParserInfo, execParser, fullDesc, header, helper, info, progDesc)
import Test.Hydra.Prelude (withTempDir)

main :: IO ()
main =
  execParser hydraClusterOptions >>= run

run :: Options -> IO ()
run options =
  withTracer (Verbose "hydra-cluster") $ \tracer -> do
    let fromCardanoNode = contramap FromCardanoNode tracer
    withStateDirectory $ \workDir ->
      case knownNetwork of
        Just network ->
          withCardanoNodeOnKnownNetwork fromCardanoNode workDir network $ \node -> do
            publishOrReuseHydraScripts tracer node
              >>= singlePartyHeadFullLifeCycle tracer workDir node
        Nothing ->
          withCardanoNodeDevnet fromCardanoNode workDir $ \node -> do
            txId <- publishOrReuseHydraScripts tracer node
            singlePartyOpenAHead tracer workDir node txId $ \HydraClient{} -> do
              forever (threadDelay 60) -- do nothing
 where
  Options{knownNetwork, stateDirectory, publishHydraScripts} = options

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
