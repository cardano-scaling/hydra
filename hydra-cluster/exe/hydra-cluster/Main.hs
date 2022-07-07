module Main where

import Hydra.Prelude

import Hydra.Cluster.Options (Options, parseOptions)
import Options.Applicative (ParserInfo, execParser, fullDesc, header, helper, info, progDesc)

main :: IO ()
main = do
  print =<< execParser hydraClusterOptions

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
