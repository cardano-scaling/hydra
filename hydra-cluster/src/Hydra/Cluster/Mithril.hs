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

data MithrilLog
  = StartSnapshotDownload {network :: KnownNetwork, directory :: FilePath}
  | -- | Output captured directly from mithril-client stderr.
    StdErr {output :: Value}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Downloads and unpacks latest snapshot for given network in db/ of given
-- directory.
downloadLatestSnapshotTo :: Tracer IO MithrilLog -> KnownNetwork -> FilePath -> IO ()
downloadLatestSnapshotTo tracer network directory = do
  traceWith tracer StartSnapshotDownload{network, directory}
  case (genesisKeyURL, ancillaryKeyURL, aggregatorEndpoint) of
    (Just genesisKeyURL', Just ancillaryKeyURL', Just aggregatorEndpoint') -> do
      genesisKey <- parseRequest genesisKeyURL' >>= httpBS <&> getResponseBody
      ancillaryKey <- parseRequest ancillaryKeyURL' >>= httpBS <&> getResponseBody
      let cmd =
            setStderr createPipe $
              proc mithrilExe $
                concat
                  [ ["--origin-tag", "HYDRA"]
                  , ["--aggregator-endpoint", aggregatorEndpoint']
                  , ["cardano-db", "download", "latest"]
                  , ["--genesis-verification-key", decodeUtf8 genesisKey]
                  , ["--ancillary-verification-key", decodeUtf8 ancillaryKey]
                  , ["--download-dir", directory]
                  , ["--json"]
                  ]
      withProcessWait_ cmd traceStderr
    _ -> error "Mithril should not be used with blockfrost chain backend"
 where
  -- Note: Minor hack; we use a different version of the mithril-client for
  -- these networks. Hopefully this can be removed, one day.
  mithrilExe = "mithril-client"

  traceStderr p =
    ignoreEOFErrors . forever $ do
      bytes <- BS.hGetLine (getStderr p)
      case Aeson.eitherDecodeStrict bytes of
        Left err -> error $ "failed to decode: \n" <> show bytes <> "\nerror: " <> show err
        Right output -> traceWith tracer StdErr{output}

  ignoreEOFErrors =
    handleJust (guard . isEOFError) (const $ pure ())

  genesisKeyURL = case network of
    Mainnet -> Just "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/genesis.vkey"
    Preproduction -> Just "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey"
    Preview -> Just "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey"
    BlockfrostPreview -> Nothing
    BlockfrostPreprod -> Nothing
    BlockfrostMainnet -> Nothing

  ancillaryKeyURL = case network of
    Mainnet -> Just "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/ancillary.vkey"
    Preproduction -> Just "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/ancillary.vkey"
    Preview -> Just "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/ancillary.vkey"
    BlockfrostPreview -> Nothing
    BlockfrostPreprod -> Nothing
    BlockfrostMainnet -> Nothing

  aggregatorEndpoint = case network of
    Mainnet -> Just "https://aggregator.release-mainnet.api.mithril.network/aggregator"
    Preproduction -> Just "https://aggregator.release-preprod.api.mithril.network/aggregator"
    Preview -> Just "https://aggregator.pre-release-preview.api.mithril.network/aggregator"
    BlockfrostPreview -> Nothing
    BlockfrostPreprod -> Nothing
    BlockfrostMainnet -> Nothing
