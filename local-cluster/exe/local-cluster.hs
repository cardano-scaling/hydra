-- | Main module for an executable to run a local cluster from the command line.
module Main where

import Hydra.Prelude

import CardanoCluster (ClusterConfig (..), withCluster)
import Hydra.Logging (Verbosity (Verbose), withTracer)
import System.IO.Temp (withSystemTempDirectory)

main :: IO ()
main = do
  withTracer (Verbose "local-cluster") show $ \tr -> do
    withSystemTempDirectory "hydra-local-cluster" $ \tmp -> do
      withCluster tr (ClusterConfig tmp) $ \_ -> do
        forever $ threadDelay 100_000 -- 100ms
