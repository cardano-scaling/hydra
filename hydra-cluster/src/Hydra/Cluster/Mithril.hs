-- | Mithril client to bootstrap cardano nodes in the hydra-cluster.
module Hydra.Cluster.Mithril where

import Hydra.Prelude

import Control.Tracer (Tracer, traceWith)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Hydra.Cluster.Fixture (KnownNetwork (..))
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import System.IO.Error (isEOFError)
import System.Process.Typed (createPipe, getStderr, proc, setStderr, withProcessWait_)
import Test.Hydra.Prelude (pendingWith)

data MithrilLog
  = StartSnapshotDownload {network :: KnownNetwork, directory :: FilePath}
  | -- | Output captured directly from mithril-client stderr.
    StdErr {output :: Value}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Downloads and unpacks latest snapshot for given network in db/ of given
-- directory.
downloadLatestSnapshotTo :: Tracer IO MithrilLog -> KnownNetwork -> FilePath -> IO ()
downloadLatestSnapshotTo tracer network directory = do
  when (network == Sanchonet) $
    pendingWith "Mithril deployment of testing-sanchonet requires mithril-client 0.7.8, which is not yet released and does not work on other networks."

  traceWith tracer StartSnapshotDownload{network, directory}
  genesisKey <- parseRequest genesisKeyURL >>= httpBS <&> getResponseBody
  let cmd =
        setStderr createPipe $
          proc "mithril-client" $
            concat
              [ ["--aggregator-endpoint", aggregatorEndpoint]
              , ["snapshot", "download", "latest"]
              , ["--genesis-verification-key", decodeUtf8 genesisKey]
              , ["--download-dir", directory]
              , ["--json"]
              ]
  withProcessWait_ cmd traceStderr
 where
  traceStderr p =
    ignoreEOFErrors . forever $ do
      bytes <- BS.hGetLine (getStderr p)
      case Aeson.eitherDecodeStrict bytes of
        Left err -> error $ "failed to decode: \n" <> show bytes <> "\nerror: " <> show err
        Right output -> traceWith tracer StdErr{output}

  ignoreEOFErrors =
    handleJust (guard . isEOFError) (const $ pure ())

  genesisKeyURL = case network of
    Mainnet -> "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/genesis.vkey"
    Preproduction -> "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey"
    Preview -> "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey"
    Sanchonet -> "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/testing-sanchonet/genesis.vkey"

  aggregatorEndpoint = case network of
    Mainnet -> "https://aggregator.release-mainnet.api.mithril.network/aggregator"
    Preproduction -> "https://aggregator.release-preprod.api.mithril.network/aggregator"
    Preview -> "https://aggregator.pre-release-preview.api.mithril.network/aggregator"
    Sanchonet -> "https://aggregator.testing-sanchonet.api.mithril.network/aggregator"
