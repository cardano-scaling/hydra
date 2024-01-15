-- | Mithril client to bootstrap cardano nodes in the hydra-cluster.
module Hydra.Cluster.Mithril where

import Hydra.Prelude

import Hydra.Cluster.Fixture (KnownNetwork (..))
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import System.Process (callProcess)

-- | Downloads and unpacks latest snapshot for given network in db/ of given
-- directory.
downloadLatestSnapshotTo :: KnownNetwork -> FilePath -> IO ()
downloadLatestSnapshotTo network dir = do
  -- TODO: Use a tracer?
  putTextLn $ "Downloading latest snapshot of " <> show network <> " to " <> show dir
  genesisKey <- parseRequest (genesisKeyURLForNetwork network) >>= httpBS <&> getResponseBody
  -- TODO: not inherit handles
  callProcess "mithril-client" $
    concat
      [ ["--aggregator-endpoint", aggregatorEndpointForNetwork network]
      , ["snapshot", "download", "latest"]
      , ["--genesis-verification-key", decodeUtf8 genesisKey]
      , ["--download-dir", dir]
      ]
 where
  genesisKeyURLForNetwork = \case
    Mainnet -> "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/genesis.vkey"
    Preproduction -> "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey"
    Preview -> "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey"

  aggregatorEndpointForNetwork = \case
    Mainnet -> "https://aggregator.release-mainnet.api.mithril.network/aggregator"
    Preproduction -> "https://aggregator.release-preprod.api.mithril.network/aggregator"
    Preview -> "https://aggregator.pre-release-preview.api.mithril.network/aggregator"
