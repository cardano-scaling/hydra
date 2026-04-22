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
--   - address: "peer1:5001"
--     cardano-verification-key: peer1.cardano.vk
--     hydra-verification-key: peer1-hydra.vk
-- api-host: "127.0.0.1"
-- api-port: 4001
-- hydra-signing-key: hydra.sk
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
module Hydra.Config (loadConfig, resolvePaths, renderConfig) where

import Hydra.Prelude

import Data.Aeson (KeyValue ((.=)), Value (..), object, withObject, (.:), (.:?))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
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
  serialiseToRawBytesHexText,
 )
import Hydra.Logging (Verbosity (..))
import Hydra.Network (Host, NodeId (..), PortNumber, WhichEtcd (..), readHost, showHost)
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
import System.FilePath (isRelative, takeDirectory, (</>))
import Test.QuickCheck (Positive (..))

-- | Load 'RunOptions' from a YAML configuration file.
--
-- Keys use kebab-case matching the CLI flag names, e.g. @node-id@, @api-host@.
-- Missing keys fall back to the same defaults as the CLI.
--
-- Relative file paths in the config are resolved relative to the directory
-- containing the config file, not the current working directory.
loadConfig :: FilePath -> IO RunOptions
loadConfig path = do
  value <- Yaml.decodeFileThrow path
  case parseEither parseRunOptions value of
    Left err -> die $ "Failed to parse config file " <> path <> ":\n  " <> err
    Right opts -> pure (resolvePaths (takeDirectory path) opts)

-- | Make all relative 'FilePath' fields in 'RunOptions' absolute by resolving
-- them against @dir@ (the directory containing the config file).  Absolute
-- paths are left unchanged.
resolvePaths :: FilePath -> RunOptions -> RunOptions
resolvePaths dir opts =
  opts
    { hydraSigningKey = resolve opts.hydraSigningKey
    , hydraVerificationKeys = map resolve opts.hydraVerificationKeys
    , persistenceDir = resolve opts.persistenceDir
    , ledgerConfig = resolveLedgerConfig opts.ledgerConfig
    , chainConfig = resolveChainConfig opts.chainConfig
    }
 where
  resolve p
    | isRelative p = dir </> p
    | otherwise = p

  resolveLedgerConfig (CardanoLedgerConfig f) =
    CardanoLedgerConfig (resolve f)

  resolveChainConfig (Cardano cfg) = Cardano (resolveCardanoChainConfig cfg)
  resolveChainConfig (Offline cfg) = Offline (resolveOfflineChainConfig cfg)

  resolveCardanoChainConfig cfg =
    cfg
      { cardanoSigningKey = resolve cfg.cardanoSigningKey
      , cardanoVerificationKeys = map resolve cfg.cardanoVerificationKeys
      , chainBackendOptions = resolveChainBackend cfg.chainBackendOptions
      }

  resolveChainBackend (Direct o) =
    Direct o{nodeSocket = case o.nodeSocket of File p -> File (resolve p)}
  resolveChainBackend (Blockfrost o) =
    Blockfrost o{projectPath = resolve o.projectPath}

  resolveOfflineChainConfig cfg =
    cfg
      { initialUTxOFile = resolve cfg.initialUTxOFile
      , ledgerGenesisFile = fmap resolve cfg.ledgerGenesisFile
      }

-- | Fail the 'Parser' if @obj@ contains any key not in @knownKeys@.
-- This catches typos and stale keys from older config files early.
checkUnknownKeys :: [Text] -> Object -> Parser ()
checkUnknownKeys knownKeys obj =
  case filter (`notElem` knownKeys) (map Key.toText (KeyMap.keys obj)) of
    [] -> pure ()
    unknown ->
      fail . toString $
        "Unknown configuration key(s): "
          <> T.intercalate ", " unknown
          <> "\nValid keys are: "
          <> T.intercalate ", " knownKeys

-- ---------------------------------------------------------------------------
-- Top-level parser

parseRunOptions :: Value -> Parser RunOptions
parseRunOptions = withObject "RunOptions" $ \o -> do
  checkUnknownKeys
    [ "quiet"
    , "node-id"
    , "listen"
    , "advertise"
    , "peers"
    , "api-host"
    , "api-port"
    , "tls-cert"
    , "tls-key"
    , "monitoring-port"
    , "hydra-signing-key"
    , "hydra-verification-keys"
    , "persistence-dir"
    , "persistence-rotate-after"
    , "chain"
    , "ledger-protocol-parameters"
    , "use-system-etcd"
    , "api-transaction-timeout"
    ]
    o
  quiet <- o .:? "quiet" .!= False
  let verbosity = if quiet then Quiet else Verbose "HydraNode"
  nodeId <- NodeId <$> (o .:? "node-id" .!= "hydra-node-1")
  listenStr <- o .:? "listen" .!= ("0.0.0.0:5001" :: String)
  listen <- parseHost "listen" listenStr
  mAdvertiseStr <- o .:? "advertise" :: Parser (Maybe String)
  advertise <- mapM (parseHost "advertise") mAdvertiseStr
  peerEntries <- o .:? "peers" .!= ([] :: [Value]) >>= mapM parsePeerEntry
  extraHydraVKs <- o .:? "hydra-verification-keys" .!= []
  let selfAddr = fromMaybe listen advertise
      filteredEntries = filter ((/= selfAddr) . (.peerHost)) peerEntries
      peers = map (.peerHost) filteredEntries
      peerCardanoVKs = mapMaybe (.peerCardanoVK) filteredEntries
      hydraVerificationKeys = mapMaybe (.peerHydraVK) filteredEntries <> extraHydraVKs
  apiHostStr <- o .:? "api-host" .!= ("127.0.0.1" :: String)
  apiHost <- parseIP "api-host" apiHostStr
  apiPort <- o .:? "api-port" .!= (4001 :: PortNumber)
  tlsCertPath <- o .:? "tls-cert"
  tlsKeyPath <- o .:? "tls-key"
  monitoringPort <- o .:? "monitoring-port"
  hydraSigningKey <- o .:? "hydra-signing-key" .!= "hydra.sk"
  persistenceDir <- o .:? "persistence-dir" .!= "./"
  persistenceRotateAfter <- fmap Positive <$> (o .:? "persistence-rotate-after" :: Parser (Maybe Natural))
  mChain <- o .:? "chain"
  chainConfig <- case mChain of
    Just v -> parseChainConfig peerCardanoVKs v
    Nothing -> pure $ applyPeerCardanoVKs peerCardanoVKs defaultRunOptions.chainConfig
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

parseChainConfig :: [FilePath] -> Value -> Parser ChainConfig
parseChainConfig peerCardanoVKs = withObject "chain" $ \o -> do
  mode <- o .:? "mode" .!= ("cardano" :: Text)
  case mode of
    "cardano" -> Cardano <$> parseCardanoChainConfig peerCardanoVKs o
    "offline" -> Offline <$> parseOfflineChainConfig o
    other -> fail $ "Unknown chain mode '" <> toString other <> "'. Expected 'cardano' or 'offline'."

parseCardanoChainConfig :: [FilePath] -> Object -> Parser CardanoChainConfig
parseCardanoChainConfig peerCardanoVKs o = do
  checkUnknownKeys
    [ "mode"
    , "network"
    , "hydra-scripts-tx-id"
    , "cardano-signing-key"
    , "cardano-verification-keys"
    , "start-chain-from"
    , "contestation-period"
    , "deposit-period"
    , "unsynced-period"
    , "backend"
    ]
    o
  hydraScriptsTxId <- parseHydraScripts o
  cardanoSigningKey <- o .:? "cardano-signing-key" .!= defaultCardanoChainConfig.cardanoSigningKey
  chainVKs <- o .:? "cardano-verification-keys" .!= []
  let cardanoVerificationKeys = peerCardanoVKs <> chainVKs
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
  checkUnknownKeys ["mode", "offline-head-seed", "initial-utxo", "ledger-genesis"] o
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
  checkUnknownKeys ["mode", "mainnet", "testnet-magic", "node-socket"] o
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
  checkUnknownKeys ["mode", "project-path", "query-timeout", "retry-timeout"] o
  projectPath <- o .:? "project-path" .!= defaultBlockfrostOptions.projectPath
  queryTimeout <- o .:? "query-timeout" .!= defaultBlockfrostOptions.queryTimeout
  retryTimeout <- o .:? "retry-timeout" .!= defaultBlockfrostOptions.retryTimeout
  pure BlockfrostOptions{projectPath, queryTimeout, retryTimeout}

-- ---------------------------------------------------------------------------
-- Helpers

data PeerEntry = PeerEntry
  { peerHost :: Host
  , peerCardanoVK :: Maybe FilePath
  , peerHydraVK :: Maybe FilePath
  }

-- | Parse a peer entry which may be a plain "HOST:PORT" string or an object
-- with @address@ and optional @cardano-verification-key@ / @hydra-verification-key@ fields.
parsePeerEntry :: Value -> Parser PeerEntry
parsePeerEntry (String s) = do
  h <- parseHost "peers" (toString s)
  pure PeerEntry{peerHost = h, peerCardanoVK = Nothing, peerHydraVK = Nothing}
parsePeerEntry v =
  withObject
    "peer"
    ( \o -> do
        checkUnknownKeys ["address", "cardano-verification-key", "hydra-verification-key"] o
        addrStr <- o .: "address"
        h <- parseHost "peers.address" addrStr
        cardanoVK <- o .:? "cardano-verification-key"
        hydraVK <- o .:? "hydra-verification-key"
        pure PeerEntry{peerHost = h, peerCardanoVK = cardanoVK, peerHydraVK = hydraVK}
    )
    v

-- | Inject peer-sourced cardano VKs into an existing 'ChainConfig'.
applyPeerCardanoVKs :: [FilePath] -> ChainConfig -> ChainConfig
applyPeerCardanoVKs [] cfg = cfg
applyPeerCardanoVKs vks (Cardano cfg) = Cardano cfg{cardanoVerificationKeys = vks <> cfg.cardanoVerificationKeys}
applyPeerCardanoVKs _ cfg = cfg

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

-- ---------------------------------------------------------------------------
-- Rendering

-- | Render 'RunOptions' as a JSON document whose structure matches the YAML
-- config file format (kebab-case keys, same hierarchy).  This is the inverse
-- of 'loadConfig' and is served at the @GET /config@ HTTP endpoint so
-- operators can inspect the effective configuration the node is running with.
renderConfig :: RunOptions -> Value
renderConfig opts =
  object $
    [ "node-id" .= opts.nodeId
    , "quiet" .= (opts.verbosity == Quiet)
    , "listen" .= showHost opts.listen
    , "api-host" .= show opts.apiHost
    , "api-port" .= opts.apiPort
    , "hydra-signing-key" .= opts.hydraSigningKey
    , "hydra-verification-keys" .= opts.hydraVerificationKeys
    , "peers" .= map showHost opts.peers
    , "persistence-dir" .= opts.persistenceDir
    , "ledger-protocol-parameters" .= ledgerParamsFile opts.ledgerConfig
    , "use-system-etcd" .= (opts.whichEtcd == SystemEtcd)
    , "api-transaction-timeout" .= opts.apiTransactionTimeout
    , "chain" .= renderChainConfig opts.chainConfig
    ]
    <> catMaybes
      [ ("advertise" .=) . showHost <$> opts.advertise
      , ("monitoring-port" .=) <$> opts.monitoringPort
      , ("tls-cert" .=) <$> opts.tlsCertPath
      , ("tls-key" .=) <$> opts.tlsKeyPath
      , ("persistence-rotate-after" .=) <$> opts.persistenceRotateAfter
      ]
 where
  ledgerParamsFile (CardanoLedgerConfig f) = f

  renderChainConfig (Cardano cfg) =
    object $
      [ "mode" .= ("cardano" :: Text)
      , "cardano-signing-key" .= cfg.cardanoSigningKey
      , "cardano-verification-keys" .= cfg.cardanoVerificationKeys
      , "contestation-period" .= cfg.contestationPeriod
      , "deposit-period" .= cfg.depositPeriod
      , "unsynced-period" .= cfg.unsyncedPeriod
      , "backend" .= renderBackend cfg.chainBackendOptions
      ]
      <> catMaybes
        [ ("start-chain-from" .=) . renderChainPoint <$> cfg.startChainFrom
        ]
      <> ["hydra-scripts-tx-id" .= map serialiseToRawBytesHexText cfg.hydraScriptsTxId | not (null cfg.hydraScriptsTxId)]
  renderChainConfig (Offline cfg) =
    object $
      [ "mode" .= ("offline" :: Text)
      , "offline-head-seed" .= serialiseToRawBytesHexText cfg.offlineHeadSeed
      , "initial-utxo" .= cfg.initialUTxOFile
      ]
      <> catMaybes [("ledger-genesis" .=) <$> cfg.ledgerGenesisFile]

  renderBackend (Direct o) =
    object $
      [ "mode" .= ("direct" :: Text)
      , "node-socket" .= (case o.nodeSocket of File p -> p)
      ]
      <> case o.networkId of
        Mainnet -> ["mainnet" .= True]
        Testnet (NetworkMagic n) -> ["testnet-magic" .= n]
  renderBackend (Blockfrost o) =
    object
      [ "mode" .= ("blockfrost" :: Text)
      , "project-path" .= o.projectPath
      , "query-timeout" .= o.queryTimeout
      , "retry-timeout" .= o.retryTimeout
      ]

  renderChainPoint :: ChainPoint -> Text
  renderChainPoint ChainPointAtGenesis = "0"
  renderChainPoint (ChainPoint (SlotNo s) h) = T.pack (show s) <> "." <> serialiseToRawBytesHexText h
