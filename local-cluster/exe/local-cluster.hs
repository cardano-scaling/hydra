-- | Main module for an executable to run a local cluster from the command line.
module Main where

import Hydra.Prelude

import CardanoCluster (ClusterConfig (..), withCluster)
import Control.Tracer (Tracer (..))
import System.IO.Temp (withSystemTempDirectory)

main :: IO ()
main = do
  withSystemTempDirectory "hydra-local-cluster" $ \tmp -> do
    withCluster (Tracer print) (ClusterConfig tmp) $ \_ -> do
      forever $ threadDelay 100_000 -- 100ms
