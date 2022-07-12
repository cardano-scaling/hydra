module Main where

import Hydra.Prelude

import CardanoNode (withCardanoNodeOnKnownNetwork)
import Hydra.Cluster.Fixture (knownNetworkId)
import Hydra.Cluster.Options (Options (..), parseOptions)
import Hydra.Cluster.Scenarios (singlePartyHeadFullLifeCycle)
import Hydra.Logging (Verbosity (Verbose), withTracer)
import HydraNode (EndToEndLog (FromCardanoNode))
import Options.Applicative (ParserInfo, execParser, fullDesc, header, helper, info, progDesc)
import Test.Hydra.Prelude (withTempDir)

main :: IO ()
main =
  execParser hydraClusterOptions >>= run

run :: Options -> IO ()
run options =
  withTracer (Verbose "hydra-cluster") $ \tracer -> do
    withStateDirectory $ \workDir ->
      withCardanoNodeOnKnownNetwork (contramap FromCardanoNode tracer) workDir knownNetwork $
        singlePartyHeadFullLifeCycle tracer workDir (knownNetworkId knownNetwork)
 where
  Options{knownNetwork, stateDirectory} = options

  withStateDirectory action = case stateDirectory of
    Nothing -> withTempDir ("hydra-cluster-" <> show knownNetwork) action
    Just sd -> action sd

hydraClusterOptions :: ParserInfo Options
hydraClusterOptions =
  info
    (parseOptions <**> helper)
    ( fullDesc
        <> progDesc
          "Starts a local cluster of interconnected Hydra nodes \
          \talking to a configurable cardano network.\n\
          \Right now, a cluster of size 1 is started and walks a \
          \simple 1 party Hydra Head through its full life cycle."
        <> header "hydra-cluster - running hydra-node clusters"
    )
