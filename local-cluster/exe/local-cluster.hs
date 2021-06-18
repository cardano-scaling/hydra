-- | Main module for an executable to run a local cluster from the command line.
module Main where

import Hydra.Prelude

import Lib (ClusterConfig (..), withCluster)
import Logging (Severity (..), withStdoutTracer)
import System.IO.Temp (withSystemTempDirectory)

main :: IO ()
main = do
  withStdoutTracer "local-cluster" Info show $ \tr -> do
    withSystemTempDirectory "hydra-local-cluster" $ \tmp -> do
      withCluster tr (ClusterConfig tmp) $ \_ -> do
        forever $ threadDelay 100000 -- 100ms
