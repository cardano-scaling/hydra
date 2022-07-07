module Main where

import Hydra.Prelude

import CardanoNode (RunningNode (RunningNode), withCardanoNodeOnKnownNetwork)
import Hydra.Cluster.Options (Options (..), parseOptions)
import Hydra.Logging (showLogsOnFailure)
import Options.Applicative (ParserInfo, execParser, fullDesc, header, helper, info, progDesc)
import Test.Hydra.Prelude (withTempDir)

main :: IO ()
main =
  execParser hydraClusterOptions >>= run

run :: Options -> IO ()
run options =
  showLogsOnFailure $ \tracer -> do
    withTempDir ("hydra-cluster-" <> show knownNetwork) $ \tempDir -> do
      withCardanoNodeOnKnownNetwork tracer tempDir knownNetwork $ \(RunningNode _ socketFile) ->
        print socketFile
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
          \Right now, a cluster of size 1 is started and runs a \
          \simple 1 party Hydra head life cycle."
        <> header "hydra-cluster - running Hydra node clusters"
    )
