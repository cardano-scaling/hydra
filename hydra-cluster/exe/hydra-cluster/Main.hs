module Main where

import Hydra.Prelude

import CardanoNode (withCardanoNodeOnKnownNetwork)
import Hydra.Cluster.Options (Options (..), parseOptions)
import Hydra.Cluster.Scenarios (knownNetworkId, singlePartyHeadFullLifeCycle)
import Hydra.Logging (showLogsOnFailure)
import Options.Applicative (ParserInfo, execParser, fullDesc, header, helper, info, progDesc)
import Test.Hydra.Prelude (withTempDir)

main :: IO ()
main =
  execParser hydraClusterOptions >>= run

run :: Options -> IO ()
run options =
  showLogsOnFailure $ \tracer -> do
    withTempDir ("hydra-cluster-" <> show knownNetwork) $ \tempDir ->
      withCardanoNodeOnKnownNetwork tracer tempDir knownNetwork $
        singlePartyHeadFullLifeCycle (knownNetworkId knownNetwork)
 where
  Options{knownNetwork} = options

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
