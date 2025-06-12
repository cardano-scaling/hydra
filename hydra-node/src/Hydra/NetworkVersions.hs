{-# LANGUAGE TemplateHaskell #-}

module Hydra.NetworkVersions where

import Hydra.Prelude hiding (encodeUtf8)

import Control.Lens ((^.), (^?))
import Data.Aeson (Value (..), encode)
import Data.Aeson.Lens (key, nonNull, _Key)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (splitOn)
import Data.Text.Encoding (encodeUtf8)
import Data.Version (Version (..), showVersion)
import Hydra.Cardano.Api (TxId, deserialiseFromRawBytesHex)
import Hydra.Version (embeddedRevision, gitRevision, unknownVersion)
import Paths_hydra_node (version)

hydraNodeVersion :: Version
hydraNodeVersion =
  version & \(Version semver _) -> Version semver revision
 where
  revision =
    maybeToList $
      embeddedRevision
        <|> gitRevision
        <|> Just unknownVersion

networkVersions :: ByteString
networkVersions = $(makeRelativeToProject "./networks.json" >>= embedFile)

parseNetworkTxIds :: MonadFail m => Version -> String -> m [TxId]
parseNetworkTxIds hydraVersion network = do
  case networkVersions ^? key (network ^. _Key) of
    Nothing -> fail $ "Unknown network: " <> toString network
    Just t -> getLastTxId t
 where
  getLastTxId t = do
    case splitOn "-" $ fromString $ showVersion hydraVersion of
      [] -> fail "Failed to parse hydra-node revision."
      (rev : _) -> do
        case encode t ^? key (rev ^. _Key) . nonNull of
          Just (String s) -> mapM parseToTxId $ splitOn "," s
          _ -> fail "Failed to find released hydra-node version in networks.json."

  parseToTxId textTxId = do
    case deserialiseFromRawBytesHex $ encodeUtf8 textTxId of
      Left _ -> fail $ "Failed to parse string to TxId: " <> toString textTxId
      Right txid -> pure txid
