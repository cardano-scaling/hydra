-- | Utilities used across hydra-cluster
module Hydra.Cluster.Util where

import Hydra.Prelude

import qualified Data.ByteString as BS
import qualified Paths_hydra_cluster as Pkg
import System.FilePath ((</>))

-- | Lookup a config file similar reading a file from disk.
readConfigFile :: FilePath -> IO ByteString
readConfigFile source = do
  filename <- Pkg.getDataFileName ("config" </> source)
  BS.readFile filename
