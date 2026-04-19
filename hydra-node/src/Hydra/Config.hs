{-# LANGUAGE OverloadedRecordDot #-}

-- | YAML-based configuration file support for hydra-node.
--
-- This module provides a user-friendly configuration file format that mirrors
-- the CLI flag names (kebab-case). A configuration file can be used instead of
-- CLI flags by passing @--config FILE@ to hydra-node.
--
-- Example configuration:
--
-- @
-- node-id: hydra-node-1
-- listen: "0.0.0.0:5001"
-- peers:
--   - "peer1:5001"
-- api-host: "127.0.0.1"
-- api-port: 4001
-- hydra-signing-key: hydra.sk
-- hydra-verification-keys:
--   - peer1.vk
-- persistence-dir: "./"
-- ledger-protocol-parameters: protocol-parameters.json
-- chain:
--   mode: cardano
--   network: preview
--   cardano-signing-key: cardano.sk
--   contestation-period: 43200
--   deposit-period: 3600
--   backend:
--     mode: direct
--     testnet-magic: 2
--     node-socket: node.socket
-- @
module Hydra.Config (loadConfig) where

import Hydra.Prelude

import Data.Aeson (Value, withObject, (.:?))
import Data.Aeson.Types (Object, Parser, parseEither, (.!=))
import Data.IP (IP)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Hydra.Cardano.Api (
  ChainPoint (..),
  File (..),
  NetworkId (..),
  NetworkMagic (..),
  SlotNo (..),
  SocketPath,
  TxId,
  deserialiseFromRawBytesHex,
 )
import Hydra.Logging (Verbosity (..))
import Hydra.Network (Host, NodeId (..), PortNumber, WhichEtcd (..), readHost)
import Hydra.NetworkVersions (hydraNodeVersion, parseNetworkTxIds)
import Hydra.Node.ApiTransactionTimeout (ApiTransactionTimeout (..))
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod (..), defaultUnsyncedPeriodFor)
import Hydra.Options (
  BlockfrostOptions (..),
  CardanoChainConfig (..),
  ChainBackendOptions (..),
  ChainConfig (..),
  DirectOptions (..),
  LedgerConfig (..),
  OfflineChainConfig (..),
  RunOptions (..),
  defaultBlockfrostOptions,
  defaultCardanoChainConfig,
  defaultContestationPeriod,
  defaultDepositPeriod,
  defaultRunOptions,
 )
import Hydra.Tx.HeadId (HeadSeed)
import Test.QuickCheck (Positive (..))

-- | Load 'RunOptions' from a YAML configuration file.
--
-- Keys use kebab-case matching the CLI flag names, e.g. @node-id@, @api-host@.
-- Missing keys fall back to the same defaults as the CLI.
loadConfig :: FilePath -> IO RunOptions
loadConfig path = do
  value <- Yaml.decodeFileThrow path
  case parseEither parseRunOptions value of
    Left err -> die $ "Failed to parse config file " <> path <> ":\n  " <> err
    Right opts -> pure opts

-- ---------------------------------------------------------------------------
-- Top-level parser

parseRunOptions :: Value -> Parser RunOptions
parseRunOptions = withObject "RunOptions" $ \o -> do
  quiet <- o .:? "quiet" .!= False
  let verbosity = if quiet then Quiet else Verbose "HydraNode"
  nodeId <- NodeId <$> (o .:? "node-id" .!= "hydra-node-1")
  listenStr <- o .:? "listen" .!= ("0.0.0.0:5001" :: String)
  listen <- parseHost "listen" listenStr
  mAdvertiseStr <- o .:? "advertise" :: Parser (Maybe String)
  advertise <- mapM (parseHost "advertise") mAdvertiseStr
  peerStrs <- o .:? "peers" .!= ([] :: [String])
  peers <- mapM (parseHost "peers") peerStrs
  apiHostStr <- o .:? "api-host" .!= ("127.0.0.1" :: String)
  apiHost <- parseIP "api-host" apiHostStr
  apiPort <- o .:? "api-port" .!= (4001 :: PortNumber)
  tlsCertPath <- o .:? "tls-cert"
  tlsKeyPath <- o .:? "tls-key"
  monitoringPort <- o .:? "monitoring-port"
  hydraSigningKey <- o .:? "hydra-signing-key" .!= "hydra.sk"
  hydraVerificationKeys <- o .:? "hydra-verification-keys" .!= []
  persistenceDir <- o .:? "persistence-dir" .!= "./"
  persistenceRotateAfter <- fmap Positive <$> (o .:? "persistence-rotate-after" :: Parser (Maybe Natural))
  chainConfig <- maybe (pure defaultRunOptions.chainConfig) parseChainConfig =<< (o .:? "chain")
  ledgerProtocolParams <- o .:? "ledger-protocol-parameters" .!= "protocol-parameters.json"
  let ledgerConfig = CardanoLedgerConfig ledgerProtocolParams
  useSystemEtcd <- o .:? "use-system-etcd" .!= False
  let whichEtcd = if useSystemEtcd then SystemEtcd else EmbeddedEtcd
  apiTransactionTimeout <- o .:? "api-transaction-timeout" .!= ApiTransactionTimeout 300
  pure
    RunOptions
      { verbosity
      , nodeId
      , listen
      , advertise
      , peers
      , apiHost
      , apiPort
      , tlsCertPath
      , tlsKeyPath
      , monitoringPort
      , hydraSigningKey
      , hydraVerificationKeys
      , persistenceDir
      , persistenceRotateAfter
      , chainConfig
      , ledgerConfig
      , whichEtcd
      , apiTransactionTimeout
      }

-- ---------------------------------------------------------------------------
-- Chain config

parseChainConfig :: Value -> Parser ChainConfig
parseChainConfig = withObject "chain" $ \o -> do
  mode <- o .:? "mode" .!= ("cardano" :: Text)
  case mode of
    "cardano" -> Cardano <$> parseCardanoChainConfig o
    "offline" -> Offline <$> parseOfflineChainConfig o
    other -> fail $ "Unknown chain mode '" <> toString other <> "'. Expected 'cardano' or 'offline'."

parseCardanoChainConfig :: Object -> Parser CardanoChainConfig
parseCardanoChainConfig o = do
  hydraScriptsTxId <- parseHydraScripts o
  cardanoSigningKey <- o .:? "cardano-signing-key" .!= defaultCardanoChainConfig.cardanoSigningKey
  cardanoVerificationKeys <- o .:? "cardano-verification-keys" .!= []
  mStartChainFrom <- o .:? "start-chain-from" :: Parser (Maybe Text)
  startChainFrom <- mapM parseChainPointText mStartChainFrom
  contestationPeriod <- o .:? "contestation-period" .!= defaultContestationPeriod
  depositPeriod <- o .:? "deposit-period" .!= defaultDepositPeriod
  mUnsyncedPeriod <- o .:? "unsynced-period" :: Parser (Maybe UnsyncedPeriod)
  let unsyncedPeriod = fromMaybe (defaultUnsyncedPeriodFor contestationPeriod) mUnsyncedPeriod
  chainBackendOptions <-
    maybe (pure defaultCardanoChainConfig.chainBackendOptions) parseChainBackend =<< (o .:? "backend")
  pure
    CardanoChainConfig
      { hydraScriptsTxId
      , cardanoSigningKey
      , cardanoVerificationKeys
      , startChainFrom
      , contestationPeriod
      , depositPeriod
      , unsyncedPeriod
      , chainBackendOptions
      }

parseHydraScripts :: Object -> Parser [TxId]
parseHydraScripts o = do
  mNetwork <- o .:? "network" :: Parser (Maybe Text)
  mTxIds <- o .:? "hydra-scripts-tx-id" :: Parser (Maybe [Text])
  case (mNetwork, mTxIds) of
    (Just network, _) ->
      parseNetworkTxIds hydraNodeVersion (toString network)
    (Nothing, Just txIdTexts) ->
      mapM parseTxId txIdTexts
    (Nothing, Nothing) ->
      pure []
 where
  parseTxId :: Text -> Parser TxId
  parseTxId t =
    case deserialiseFromRawBytesHex (encodeUtf8 t) of
      Left err -> fail $ "Invalid hydra-scripts-tx-id '" <> toString t <> "': " <> show err
      Right txId -> pure txId

parseOfflineChainConfig :: Object -> Parser OfflineChainConfig
parseOfflineChainConfig o = do
  mSeedText <- o .:? "offline-head-seed" :: Parser (Maybe Text)
  offlineHeadSeed <- maybe (fail "offline mode requires 'offline-head-seed'") parseHeadSeed mSeedText
  initialUTxOFile <- o .:? "initial-utxo" .!= "utxo.json"
  ledgerGenesisFile <- o .:? "ledger-genesis"
  pure OfflineChainConfig{offlineHeadSeed, initialUTxOFile, ledgerGenesisFile}

-- ---------------------------------------------------------------------------
-- Chain backend

parseChainBackend :: Value -> Parser ChainBackendOptions
parseChainBackend = withObject "backend" $ \o -> do
  mode <- o .:? "mode" .!= ("direct" :: Text)
  case mode of
    "direct" -> Direct <$> parseDirectOptions o
    "blockfrost" -> Blockfrost <$> parseBlockfrostOptions o
    other -> fail $ "Unknown backend mode '" <> toString other <> "'. Expected 'direct' or 'blockfrost'."

parseDirectOptions :: Object -> Parser DirectOptions
parseDirectOptions o = do
  mainnet <- o .:? "mainnet" .!= False
  networkId <-
    if mainnet
      then pure Mainnet
      else Testnet . NetworkMagic <$> (o .:? "testnet-magic" .!= 42)
  nodeSocketStr <- o .:? "node-socket" .!= "node.socket"
  let nodeSocket = File nodeSocketStr :: SocketPath
  pure DirectOptions{networkId, nodeSocket}

parseBlockfrostOptions :: Object -> Parser BlockfrostOptions
parseBlockfrostOptions o = do
  projectPath <- o .:? "project-path" .!= defaultBlockfrostOptions.projectPath
  queryTimeout <- o .:? "query-timeout" .!= defaultBlockfrostOptions.queryTimeout
  retryTimeout <- o .:? "retry-timeout" .!= defaultBlockfrostOptions.retryTimeout
  pure BlockfrostOptions{projectPath, queryTimeout, retryTimeout}

-- ---------------------------------------------------------------------------
-- Helpers

parseHost :: MonadFail m => String -> String -> m Host
parseHost fieldName str =
  case readHost str of
    Nothing -> fail $ "Invalid '" <> fieldName <> "' value '" <> str <> "'. Expected HOST:PORT format."
    Just h -> pure h

parseIP :: MonadFail m => String -> String -> m IP
parseIP fieldName str =
  case readMaybe str of
    Just ip -> pure ip
    Nothing -> fail $ "Invalid '" <> fieldName <> "' value '" <> str <> "'. Expected an IP address."

parseHeadSeed :: MonadFail m => Text -> m HeadSeed
parseHeadSeed t =
  case deserialiseFromRawBytesHex (encodeUtf8 t) of
    Left err -> fail $ "Invalid offline-head-seed '" <> toString t <> "': " <> show err
    Right seed -> pure seed

parseChainPointText :: MonadFail m => Text -> m ChainPoint
parseChainPointText "0" = pure ChainPointAtGenesis
parseChainPointText t =
  case T.splitOn "." t of
    [slotTxt, hashTxt] -> do
      slotNo <- case readMaybe (toString slotTxt) of
        Just n -> pure $ SlotNo n
        Nothing -> fail $ "Invalid slot number in start-chain-from: '" <> toString slotTxt <> "'"
      headerHash <- case deserialiseFromRawBytesHex (encodeUtf8 hashTxt) of
        Left err -> fail $ "Invalid block hash in start-chain-from: " <> show err
        Right h -> pure h
      pure $ ChainPoint slotNo headerHash
    _ ->
      fail $ "Invalid start-chain-from '" <> toString t <> "'. Expected format: SLOT.HEADER_HASH"
