{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Options (
  module Hydra.Options,
  ParserResult (..),
  renderFailure,
) where

import "hydra-prelude" Hydra.Prelude
import "aeson" Data.Aeson (Value (Object, String), withObject, (.:))
import "base" Control.Arrow (left)
import "lens" Control.Lens ((?~))
import "lens-aeson" Data.Aeson.Lens (atKey)
import "QuickCheck" Test.QuickCheck (Positive (..))
import "base" Data.Version (showVersion)
import "bytestring" Data.ByteString.Char8 qualified as BSC
import "hydra-cardano-api" Hydra.Cardano.Api (
  ChainPoint (..),
  File (..),
  NetworkId (..),
  NetworkMagic (..),
  SlotNo (..),
  SocketPath,
  TxId (..),
  deserialiseFromRawBytesHex,
  serialiseToRawBytesHexText,
 )
import "hydra-plutus" Hydra.Contract qualified as Contract
import "hydra-tx" Hydra.Ledger.Cardano ()
import "hydra-tx" Hydra.Tx.ContestationPeriod (ContestationPeriod, fromNominalDiffTime)
import "hydra-tx" Hydra.Tx.HeadId (HeadSeed)
import "iproute" Data.IP (IP (IPv4), toIPv4)
import "optparse-applicative" Options.Applicative (

import Hydra.Chain (maximumNumberOfParties)
import Hydra.Logging (Verbosity (..))
import Hydra.Network (Host (..), NodeId (NodeId), PortNumber, WhichEtcd (..), readHost, readPort, showHost)
import Hydra.NetworkVersions (hydraNodeVersion, parseNetworkTxIds)
import Hydra.Node.ApiTransactionTimeout (ApiTransactionTimeout (..))
import Hydra.Node.DepositPeriod (DepositPeriod (..))
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod (..), defaultUnsyncedPeriodFor)
  Parser,
  ParserInfo,
  ParserResult (..),
  auto,
  command,
  completer,
  defaultPrefs,
  eitherReader,
  execParserPure,
  flag,
  flag',
  footer,
  fullDesc,
  handleParseResult,
  header,
  help,
  helper,
  hsubparser,
  info,
  infoOption,
  listCompleter,
  long,
  maybeReader,
  metavar,
  option,
  progDesc,
  progDescDoc,
  renderFailure,
  short,
  showDefault,
  strOption,
  value,
 )
import "optparse-applicative" Options.Applicative.Builder (str)
import "optparse-applicative" Options.Applicative.Help (vsep)
import "text" Data.Text (unpack)
import "text" Data.Text qualified as T

data Command
  = Run RunOptions
  | Publish PublishOptions
  | GenHydraKey GenerateKeyPair
  deriving stock (Show, Eq)

commandParser :: Parser Command
commandParser =
  subcommands
    <|> Run <$> runOptionsParser
 where
  subcommands =
    hsubparser $
      publishScriptsCommand
        <> genHydraKeyCommand

  publishScriptsCommand =
    command
      "publish-scripts"
      ( info
          (Publish <$> publishOptionsParser)
          ( fullDesc
              <> progDescDoc
                ( Just $
                    vsep
                      [ "Publish Hydra's Plutus scripts on chain to be used"
                      , "by the hydra-node as --hydra-script-tx-id."
                      , ""
                      , " ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓ "
                      , " ┃              ⚠ WARNING ⚠              ┃ "
                      , " ┣═══════════════════════════════════════┫ "
                      , " ┃    This costs money. About 50 Ada.    ┃ "
                      , " ┃ Spent using the provided signing key. ┃ "
                      , " ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ "
                      ]
                )
              <> footer
                "The command outputs the transaction id (in base16) \
                \of the publishing transaction. This transaction id \
                \can then be passed onto '--hydra-scripts-tx-id' to \
                \start a hydra-node using the referenced scripts."
          )
      )

  genHydraKeyCommand =
    command
      "gen-hydra-key"
      ( info
          (GenHydraKey . GenerateKeyPair <$> outputFileParser)
          (progDesc "Generate a pair of Hydra signing/verification keys (off-chain keys).")
      )

data PublishOptions = PublishOptions
  { chainBackendOptions :: ChainBackendOptions
  , publishSigningKey :: FilePath
  }
  deriving stock (Show, Eq, Generic)

-- | Default options as they should also be provided by 'runOptionsParser'.
defaultPublishOptions :: PublishOptions
defaultPublishOptions =
  PublishOptions
    { chainBackendOptions = Direct defaultDirectOptions
    , publishSigningKey = "cardano.sk"
    }

defaultDirectOptions :: DirectOptions
defaultDirectOptions =
  DirectOptions
    { networkId = Testnet (NetworkMagic 42)
    , nodeSocket = "node.socket"
    }

data ChainBackendOptions
  = Direct DirectOptions
  | Blockfrost BlockfrostOptions
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data DirectOptions = DirectOptions
  { networkId :: NetworkId
  -- ^ Network identifier to which we expect to connect.
  , nodeSocket :: SocketPath
  -- ^ Path to a domain socket used to connect to the server.
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data BlockfrostOptions = BlockfrostOptions
  { projectPath :: FilePath
  -- ^ Path to the blockfrost project file
  , queryTimeout :: Int
  , retryTimeout :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

defaultBlockfrostOptions :: BlockfrostOptions
defaultBlockfrostOptions =
  BlockfrostOptions
    { projectPath = "blockfrost-project.txt"
    , queryTimeout = defaultBFQueryTimeout
    , retryTimeout = defaultBFRetryTimeout
    }

defaultBFQueryTimeout :: Int
defaultBFQueryTimeout = 30

defaultBFRetryTimeout :: Int
defaultBFRetryTimeout = 300

publishOptionsParser :: Parser PublishOptions
publishOptionsParser =
  PublishOptions <$> chainBackendOptionsParser <*> cardanoSigningKeyFileParser

data RunOptions = RunOptions
  { verbosity :: Verbosity
  , nodeId :: NodeId
  , listen :: Host
  , advertise :: Maybe Host
  , peers :: [Host]
  , apiHost :: IP
  , apiPort :: PortNumber
  , tlsCertPath :: Maybe FilePath
  , tlsKeyPath :: Maybe FilePath
  , monitoringPort :: Maybe PortNumber
  , hydraSigningKey :: FilePath
  , hydraVerificationKeys :: [FilePath]
  , persistenceDir :: FilePath
  , persistenceRotateAfter :: Maybe (Positive Natural)
  , chainConfig :: ChainConfig
  , ledgerConfig :: LedgerConfig
  , whichEtcd :: WhichEtcd
  , apiTransactionTimeout :: ApiTransactionTimeout
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Orphan instances
instance ToJSON a => ToJSON (Positive a) where
  toJSON (Positive a) = toJSON a

instance FromJSON a => FromJSON (Positive a) where
  parseJSON v = Positive <$> parseJSON v

-- | Default options as they should also be provided by 'runOptionsParser'.
defaultRunOptions :: RunOptions
defaultRunOptions =
  RunOptions
    { verbosity = Verbose "HydraNode"
    , nodeId = NodeId "hydra-node-1"
    , listen = Host "0.0.0.0" 5001
    , advertise = Nothing
    , peers = []
    , apiHost = localhost
    , apiPort = 4001
    , tlsCertPath = Nothing
    , tlsKeyPath = Nothing
    , monitoringPort = Nothing
    , hydraSigningKey = "hydra.sk"
    , hydraVerificationKeys = []
    , persistenceDir = "./"
    , persistenceRotateAfter = Nothing
    , chainConfig = Cardano defaultCardanoChainConfig
    , ledgerConfig = defaultLedgerConfig
    , whichEtcd = EmbeddedEtcd
    , apiTransactionTimeout = 300
    }
 where
  localhost = IPv4 $ toIPv4 [127, 0, 0, 1]

-- | Parser for running the cardano-node with all its 'RunOptions'.
runOptionsParser :: Parser RunOptions
runOptionsParser =
  RunOptions
    <$> verbosityParser
    <*> nodeIdParser
    <*> listenParser
    <*> optional advertiseParser
    <*> many peerParser
    <*> apiHostParser
    <*> apiPortParser
    <*> optional tlsCertPathParser
    <*> optional tlsKeyPathParser
    <*> optional monitoringPortParser
    <*> hydraSigningKeyFileParser
    <*> many hydraVerificationKeyFileParser
    <*> persistenceDirParser
    <*> optional persistenceRotateAfterParser
    <*> chainConfigParser
    <*> ledgerConfigParser
    <*> whichEtcdParser
    <*> apiTransactionTimeoutParser

whichEtcdParser :: Parser WhichEtcd
whichEtcdParser =
  flag
    EmbeddedEtcd
    SystemEtcd
    ( long "use-system-etcd"
        <> help "Use the `etcd` binary found on the path instead of the embedded one."
    )

chainConfigParser :: Parser ChainConfig
chainConfigParser =
  Cardano <$> cardanoChainConfigParser
    <|> Offline <$> offlineChainConfigParser

chainBackendOptionsParser :: Parser ChainBackendOptions
chainBackendOptionsParser = directOptionsParser <|> blockfrostOptionsParser
 where
  directOptionsParser =
    fmap Direct $
      DirectOptions
        <$> networkIdParser
        <*> nodeSocketParser

  blockfrostOptionsParser =
    fmap Blockfrost $
      BlockfrostOptions
        <$> blockfrostProjectPathParser
        <*> blockfrostQueryTimeoutParser
        <*> blockfrostRetryTimeoutParser

newtype GenerateKeyPair = GenerateKeyPair
  { outputFile :: FilePath
  }
  deriving stock (Eq, Show)

outputFileParser :: Parser FilePath
outputFileParser =
  strOption
    ( long "output-file"
        <> metavar "FILE"
        <> value "hydra-key"
        <> help "Basename of files to generate key-pair into. Signing key will be suffixed '.sk' and verification key '.vk'"
    )

newtype LedgerConfig = CardanoLedgerConfig
  { cardanoLedgerProtocolParametersFile :: FilePath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

defaultLedgerConfig :: LedgerConfig
defaultLedgerConfig =
  CardanoLedgerConfig
    { cardanoLedgerProtocolParametersFile = "protocol-parameters.json"
    }

ledgerConfigParser :: Parser LedgerConfig
ledgerConfigParser =
  CardanoLedgerConfig
    <$> cardanoLedgerProtocolParametersParser

cardanoLedgerProtocolParametersParser :: Parser FilePath
cardanoLedgerProtocolParametersParser =
  strOption
    ( long "ledger-protocol-parameters"
        <> metavar "FILE"
        <> value "protocol-parameters.json"
        <> showDefault
        <> help
          "Path to protocol parameters used in the Hydra Head. \
          \See manual how to configure this."
    )

data ChainConfig
  = Offline OfflineChainConfig
  | Cardano CardanoChainConfig
  deriving stock (Eq, Show, Generic)

instance ToJSON ChainConfig where
  toJSON = \case
    Offline cfg -> toJSON cfg & atKey "tag" ?~ String "OfflineChainConfig"
    Cardano cfg -> toJSON cfg & atKey "tag" ?~ String "CardanoChainConfig"

instance FromJSON ChainConfig where
  parseJSON =
    withObject "ChainConfig" $ \o ->
      o .: "tag" >>= \case
        "OfflineChainConfig" -> Offline <$> parseJSON (Object o)
        "CardanoChainConfig" -> Cardano <$> parseJSON (Object o)
        tag -> fail $ "unexpected tag " <> tag

data OfflineChainConfig = OfflineChainConfig
  { offlineHeadSeed :: HeadSeed
  -- ^ Manually provided seed of the offline head.
  , initialUTxOFile :: FilePath
  -- ^ Path to a json encoded starting 'UTxO' for the offline-mode head.
  , ledgerGenesisFile :: Maybe FilePath
  -- ^ Path to a shelley genesis file with slot lengths used by the offline-mode chain.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardanoChainConfig = CardanoChainConfig
  { hydraScriptsTxId :: [TxId]
  -- ^ Identifier of transaction holding the hydra scripts to use.
  , cardanoSigningKey :: FilePath
  -- ^ Path to the cardano signing key of the internal wallet.
  , cardanoVerificationKeys :: [FilePath]
  -- ^ Paths to other node's verification keys.
  , startChainFrom :: Maybe ChainPoint
  -- ^ Point at which to start following the chain.
  , contestationPeriod :: ContestationPeriod
  , depositPeriod :: DepositPeriod
  , unsyncedPeriod :: UnsyncedPeriod
  -- ^ Period of time after which we consider the node becoming unsynced with the chain.
  -- Defaults to half of the contestation period if not specified via CLI.
  , chainBackendOptions :: ChainBackendOptions
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

defaultCardanoChainConfig :: CardanoChainConfig
defaultCardanoChainConfig =
  CardanoChainConfig
    { hydraScriptsTxId = []
    , cardanoSigningKey = "cardano.sk"
    , cardanoVerificationKeys = []
    , startChainFrom = Nothing
    , contestationPeriod = defaultContestationPeriod
    , depositPeriod = defaultDepositPeriod
    , unsyncedPeriod = defaultUnsyncedPeriod
    , chainBackendOptions = Direct defaultDirectOptions
    }

data BlockfrostChainConfig = BlockfrostChainConfig
  { projectPath :: FilePath
  -- ^ Path to the blockfrost project file
  , cardanoSigningKey :: FilePath
  -- ^ Path to the cardano signing key of the internal wallet.
  , hydraScriptsTxId :: [TxId]
  -- ^ Identifier of transaction holding the hydra scripts to use.
  }
  deriving stock (Eq, Show, Generic)

offlineChainConfigParser :: Parser OfflineChainConfig
offlineChainConfigParser =
  OfflineChainConfig
    <$> offlineHeadSeedParser
    <*> initialUTxOFileParser
    <*> ledgerGenesisFileParser

offlineHeadSeedParser :: Parser HeadSeed
offlineHeadSeedParser =
  option
    (eitherReader $ left show . deserialiseFromRawBytesHex . BSC.pack)
    ( long "offline-head-seed"
        <> metavar "HEX"
        <> help "Offline mode: Hexadecimal seed bytes to derive the offline head id from. Needs to be consistent across the hydra-node instances."
    )

initialUTxOFileParser :: Parser FilePath
initialUTxOFileParser =
  option
    str
    ( long "initial-utxo"
        <> metavar "FILE"
        <> value "utxo.json"
        <> showDefault
        <> help "Offline mode: File containing initial UTxO for the L2 ledger in offline mode."
    )

ledgerGenesisFileParser :: Parser (Maybe FilePath)
ledgerGenesisFileParser =
  option
    (optional str)
    ( long "ledger-genesis"
        <> metavar "FILE"
        <> value Nothing
        <> showDefault
        <> help "Offline mode: File containing shelley genesis parameters for the simulated L1 chain in offline mode."
    )

cardanoChainConfigParser :: Parser CardanoChainConfig
cardanoChainConfigParser =
  mkCardanoChainConfig
    <$> ((hydraScriptsTxIdsParser <|> many hydraScriptsTxIdParser) <|> hydraScriptsDefaultParser)
    <*> cardanoSigningKeyFileParser
    <*> many cardanoVerificationKeyFileParser
    <*> optional startChainFromParser
    <*> contestationPeriodParser
    <*> depositPeriodParser
    <*> optional unsyncedPeriodParser
    <*> chainBackendOptionsParser
 where
  mkCardanoChainConfig hydraScriptsTxId cardanoSigningKey cardanoVerificationKeys startChainFrom contestationPeriod depositPeriod maybeUnsyncedPeriod chainBackendOptions =
    CardanoChainConfig
      { hydraScriptsTxId
      , cardanoSigningKey
      , cardanoVerificationKeys
      , startChainFrom
      , contestationPeriod
      , depositPeriod
      , unsyncedPeriod = fromMaybe (defaultUnsyncedPeriodFor contestationPeriod) maybeUnsyncedPeriod
      , chainBackendOptions
      }

blockfrostProjectPathParser :: Parser FilePath
blockfrostProjectPathParser =
  strOption
    ( long "blockfrost"
        <> metavar "FILE"
        <> showDefault
        <> value "blockfrost.txt"
        <> help
          "Blockfrost project path containing the api key."
    )

blockfrostQueryTimeoutParser :: Parser Int
blockfrostQueryTimeoutParser =
  option
    auto
    ( long "blockfrost-query-timeout"
        <> metavar "SECONDS"
        <> value defaultBFQueryTimeout
        <> showDefault
        <> help "Timeout for single queries to the Blockfrost API, in seconds."
    )

blockfrostRetryTimeoutParser :: Parser Int
blockfrostRetryTimeoutParser =
  option
    auto
    ( long "blockfrost-retry-timeout"
        <> metavar "SECONDS"
        <> value 300
        <> showDefault
        <> help "Timeout for retrying queries to the Blockfrost API, in seconds."
    )

networkIdParser :: Parser NetworkId
networkIdParser = pMainnet <|> fmap Testnet pTestnetMagic
 where
  pMainnet :: Parser NetworkId
  pMainnet =
    flag'
      Mainnet
      ( long "mainnet"
          <> help "Use the mainnet magic id."
      )

  pTestnetMagic :: Parser NetworkMagic
  pTestnetMagic =
    NetworkMagic
      <$> option
        auto
        ( long "testnet-magic"
            <> metavar "NATURAL"
            <> value 42
            <> showDefault
            <> completer (listCompleter ["1", "2", "42"])
            <> help
              "Network identifier for a testnet to connect to. We only need to \
              \provide the magic number here. For example: '2' is the 'preview' \
              \network. See https://book.world.dev.cardano.org/environments.html for available networks."
        )

nodeSocketParser :: Parser SocketPath
nodeSocketParser =
  strOption
    ( long "node-socket"
        <> metavar "FILE"
        <> value defaultDirectOptions.nodeSocket
        <> showDefault
        <> help
          "Filepath to local unix domain socket used to communicate with \
          \the cardano node."
    )

cardanoSigningKeyFileParser :: Parser FilePath
cardanoSigningKeyFileParser =
  strOption
    ( long "cardano-signing-key"
        <> metavar "FILE"
        <> showDefault
        <> value defaultCardanoChainConfig.cardanoSigningKey
        <> help
          "Cardano signing key of our hydra-node. This will be used to authorize \
          \Hydra protocol transactions for heads the node takes part in and any \
          \funds owned by this key will be used as 'fuel'."
    )

cardanoVerificationKeyFileParser :: Parser FilePath
cardanoVerificationKeyFileParser =
  option
    str
    ( long "cardano-verification-key"
        <> metavar "FILE"
        <> help
          ( "Cardano verification key of another party in the Head. Can be \
            \provided multiple times, once for each participant (current maximum limit is "
              <> show maximumNumberOfParties
              <> ")."
          )
    )

hydraSigningKeyFileParser :: Parser FilePath
hydraSigningKeyFileParser =
  option
    str
    ( long "hydra-signing-key"
        <> metavar "FILE"
        <> value "hydra.sk"
        <> showDefault
        <> help "Hydra signing key used by our hydra-node."
    )

hydraVerificationKeyFileParser :: Parser FilePath
hydraVerificationKeyFileParser =
  option
    str
    ( long "hydra-verification-key"
        <> metavar "FILE"
        <> help
          ( "Hydra verification key of another party in the Head. Can be \
            \provided multiple times, once for each participant (current maximum limit is "
              <> show maximumNumberOfParties
              <> " )."
          )
    )

peerParser :: Parser Host
peerParser =
  option
    (maybeReader readHost)
    ( long "peer"
        <> short 'P'
        <> help
          ( "A peer address in the form <host>:<port>, where <host> can be an IP \
            \address, or a host name. Can be provided multiple times, once for \
            \each peer (current maximum limit is "
              <> show maximumNumberOfParties
              <> " peers)."
          )
    )

nodeIdParser :: Parser NodeId
nodeIdParser =
  option
    str
    ( long "node-id"
        <> short 'n'
        <> metavar "NODE-ID"
        <> value "hydra-node-1"
        <> help
          "The Hydra node identifier used on the Hydra network. It is \
          \important to have a unique identifier in order to be able to \
          \distinguish between connected peers."
    )

verbosityParser :: Parser Verbosity
verbosityParser =
  flag
    (Verbose "HydraNode")
    Quiet
    ( long "quiet"
        <> short 'q'
        <> help "Turns off logging."
    )

listenParser :: Parser Host
listenParser =
  option
    auto
    ( long "listen"
        <> short 'l'
        <> value (Host "0.0.0.0" 5001)
        <> showDefault
        <> metavar "HOST:PORT"
        <> help "Address and port to listen for Hydra network connections. If --advertise is not set, this will be also advertised to other peers on the network."
    )

advertiseParser :: Parser Host
advertiseParser =
  option
    auto
    ( long "advertise"
        <> metavar "HOST:PORT"
        <> help "Address and port to advertise as public endpoint to other peers on the Hydra network. If this is not set, the --listen address is used."
    )

apiHostParser :: Parser IP
apiHostParser =
  option
    auto
    ( long "api-host"
        <> value "127.0.0.1"
        <> metavar "IP"
        <> showDefault
        <> help "Listen address for incoming client API connections."
    )

apiPortParser :: Parser PortNumber
apiPortParser =
  option
    (maybeReader readPort)
    ( long "api-port"
        <> value 4001
        <> showDefault
        <> metavar "PORT"
        <> help "Listen port for incoming client API connections."
    )

tlsCertPathParser :: Parser FilePath
tlsCertPathParser =
  option
    str
    ( long "tls-cert"
        <> metavar "FILE"
        <> help
          "Path to the TLS certificate (chain). If this and --tls-key are \
          \set, the API server will expect TLS connections (WSS/HTTPS)."
    )

tlsKeyPathParser :: Parser FilePath
tlsKeyPathParser =
  option
    str
    ( long "tls-key"
        <> metavar "FILE"
        <> help
          "Path to the TLS key. If this and --tls-cert are \
          \set, the API server will expect TLS connections (WSS/HTTPS)."
    )

monitoringPortParser :: Parser PortNumber
monitoringPortParser =
  option
    (maybeReader readPort)
    ( long "monitoring-port"
        <> metavar "PORT"
        <> help
          "Listen port for monitoring and metrics via prometheus. If left \
          \empty, monitoring server is not started."
    )

defaultApiTransactionTimeout :: ApiTransactionTimeout
defaultApiTransactionTimeout = ApiTransactionTimeout 300

apiTransactionTimeoutParser :: Parser ApiTransactionTimeout
apiTransactionTimeoutParser =
  option
    (ApiTransactionTimeout <$> auto)
    ( long "api-transaction-timeout"
        <> metavar "SECONDS"
        <> value defaultApiTransactionTimeout
        <> showDefault
        <> completer (listCompleter ["3600", "7200", "43200"])
        <> help
          "Timeout for API transactions in seconds. If a transaction \
          \takes longer than this, it will be cancelled."
    )

startChainFromParser :: Parser ChainPoint
startChainFromParser =
  option
    (maybeReader readChainPoint)
    ( long "start-chain-from"
        <> metavar "SLOT.HEADER_HASH"
        <> help
          "The id of the block we want to start observing the chain from. Only \
          \used if the last known head state is older than given point. If not \
          \given and no known head state, the chain tip is used. Composed by the \
          \slot number, a separator ('.') and the hash of the block header. For \
          \example: \
          \52970883.d36a9936ae7a07f5f4bdc9ad0b23761cb7b14f35007e54947e27a1510f897f04."
    )
 where
  readChainPoint :: String -> Maybe ChainPoint
  readChainPoint = \case
    "0" -> Just ChainPointAtGenesis
    chainPointStr ->
      case T.splitOn "." (toText chainPointStr) of
        [slotNoTxt, headerHashTxt] -> do
          slotNo <- SlotNo <$> readMaybe (toString slotNoTxt)
          headerHash <-
            either (const Nothing) Just $
              deserialiseFromRawBytesHex (encodeUtf8 headerHashTxt)
          pure $ ChainPoint slotNo headerHash
        _emptyOrSingularList ->
          Nothing

hydraScriptsTxIdsParser :: Parser [TxId]
hydraScriptsTxIdsParser =
  option
    (eitherReader $ left show . parseFromHex . BSC.split ',' . BSC.pack)
    ( long "hydra-scripts-tx-id"
        <> metavar "TXID"
        <> help
          "The transactions which are expected to have published Hydra scripts as \
          \reference scripts in their outputs. You can use the 'publish-scripts' \
          \sub-command to publish scripts yourself."
    )
 where
  parseFromHex = mapM deserialiseFromRawBytesHex

hydraScriptsTxIdParser :: Parser TxId
hydraScriptsTxIdParser =
  option
    (eitherReader $ left show . deserialiseFromRawBytesHex . BSC.pack)
    ( long "hydra-scripts-tx-id"
        <> metavar "TXID"
        <> help
          "The transaction which is expected to have published Hydra scripts as \
          \reference scripts in its outputs. Note: All scripts need to be in the \
          \first 10 outputs. See release notes for pre-published versions. You \
          \can use the 'publish-scripts' sub-command to publish them yourself."
    )

hydraScriptsDefaultParser :: Parser [TxId]
hydraScriptsDefaultParser =
  option
    (eitherReader validateNetwork)
    ( long "network"
        <> metavar "NETWORK"
        <> help "Uses the last pre-published hydra scripts for the given network."
    )
 where
  validateNetwork arg =
    case arg of
      "preview" -> parseNetworkTxIds hydraNodeVersion arg
      "preprod" -> parseNetworkTxIds hydraNodeVersion arg
      "mainnet" -> parseNetworkTxIds hydraNodeVersion arg
      _ -> Left $ "Unknown network: " <> arg

persistenceDirParser :: Parser FilePath
persistenceDirParser =
  option
    str
    ( long "persistence-dir"
        <> metavar "DIR"
        <> value "./"
        <> help
          "The directory where the Hydra Head state is stored.\
          \Do not edit these files manually!"
    )

persistenceRotateAfterParser :: Parser (Positive Natural)
persistenceRotateAfterParser =
  option
    (eitherReader validateRotateAfter)
    ( long "persistence-rotate-after"
        <> metavar "NATURAL"
        <> help
          "The number of Hydra events to trigger rotation (default: no rotation).\
          \Note it must be a positive number."
    )
 where
  validateRotateAfter :: String -> Either String (Positive Natural)
  validateRotateAfter arg =
    case readMaybe arg of
      Just n | n > 0 -> Right (Positive n)
      _ -> Left "--persistence-rotate-after must be a positive number"

hydraNodeCommand :: ParserInfo Command
hydraNodeCommand =
  info
    ( commandParser
        <**> versionInfo
        <**> hydraScriptCatalogue
        <**> helper
    )
    ( fullDesc
        <> progDesc "Starts a Hydra Node"
        <> header "hydra-node - Implementation of the Hydra Head protocol"
    )
 where
  versionInfo :: Parser (a -> a)
  versionInfo =
    infoOption
      (showVersion hydraNodeVersion)
      (long "version" <> help "Show version")

  hydraScriptCatalogue :: Parser (a -> a)
  hydraScriptCatalogue =
    infoOption
      (decodeUtf8 $ encodePretty Contract.hydraScriptCatalogue)
      (long "hydra-script-catalogue" <> help "Dump Hydra script catalogue as JSON")

-- | Default contestation period of 12 hours, aligned with Cardano's safe zone
-- on mainnet. The safe zone is approximately 3 * k / f where k = 2160 (security
-- parameter) and f = 0.05 (active slot coefficient), ensuring finality guarantees.
-- See: https://github.com/cardano-scaling/hydra/issues/2389
defaultContestationPeriod :: ContestationPeriod
defaultContestationPeriod = 43200 -- 12 hours in seconds

-- | Default unsynced period, computed as half of the default contestation period.
defaultUnsyncedPeriod :: UnsyncedPeriod
defaultUnsyncedPeriod = defaultUnsyncedPeriodFor defaultContestationPeriod

contestationPeriodParser :: Parser ContestationPeriod
contestationPeriodParser =
  option
    (auto >>= fromNominalDiffTime)
    ( long "contestation-period"
        <> metavar "SECONDS"
        <> value defaultContestationPeriod
        <> showDefault
        <> completer (listCompleter ["3600", "43200", "86400"])
        <> help
          "Contestation period for close transaction in seconds. \
          \ If this value is not in sync with other participants hydra-node will ignore the initial tx.\
          \ WARNING: On mainnet, this value should be at least 12 hours (43200s) to ensure safety\
          \ against long chain forks. Shorter periods may not provide sufficient time for dispute\
          \ resolution. See https://github.com/cardano-scaling/hydra/issues/2389 for details."
    )

defaultDepositPeriod :: DepositPeriod
defaultDepositPeriod = DepositPeriod 3600

depositPeriodParser :: Parser DepositPeriod
depositPeriodParser =
  option
    (DepositPeriod <$> auto)
    ( long "deposit-period"
        <> metavar "SECONDS"
        <> value defaultDepositPeriod
        <> showDefault
        <> completer (listCompleter ["3600", "7200", "43200"])
        <> help
          "Minimum time before deadline to consider deposits. 2 x deposit-period \
          \is used to set the deadline on any drafted deposit transactions."
    )

unsyncedPeriodParser :: Parser UnsyncedPeriod
unsyncedPeriodParser =
  option
    (UnsyncedPeriod <$> auto)
    ( long "unsynced-period"
        <> metavar "SECONDS"
        <> completer (listCompleter ["1800", "3600", "21600"])
        <> help
          "Period of time after which we consider the node becoming unsynced \
          \with the chain. Beyond this period the node will refuse to process \
          \new transactions and signing snapshots. If not provided, defaults to \
          \half of the contestation period."
    )

data InvalidOptions
  = MaximumNumberOfPartiesExceeded
  | CardanoAndHydraKeysMismatch
  deriving stock (Eq, Show)

-- | Validate cmd line arguments for hydra-node and check if they make sense before actually running the node.
-- Rules we apply:
--  - Check if number of parties is bigger than our hardcoded limit
--      (by looking at loaded hydra or cardano keys and comparing it to the 'maximumNumberOfParties')
--  - Check that number of loaded hydra keys match with the number of loaded cardano keys
--      (by comparing lengths of the two lists)
validateRunOptions :: RunOptions -> Either InvalidOptions ()
validateRunOptions RunOptions{hydraVerificationKeys, chainConfig} =
  case chainConfig of
    Offline{} -> Right ()
    Cardano CardanoChainConfig{cardanoVerificationKeys}
      | max (length hydraVerificationKeys) (length cardanoVerificationKeys) + 1 > maximumNumberOfParties ->
          Left MaximumNumberOfPartiesExceeded
      | length cardanoVerificationKeys /= length hydraVerificationKeys ->
          Left CardanoAndHydraKeysMismatch
      | otherwise -> Right ()

-- | Parse command-line arguments into a `Option` or exit with failure and error message.
parseHydraCommand :: IO Command
parseHydraCommand = getArgs <&> parseHydraCommandFromArgs >>= handleParseResult

-- | Pure parsing of `Option` from a list of arguments.
parseHydraCommandFromArgs :: [String] -> ParserResult Command
parseHydraCommandFromArgs = execParserPure defaultPrefs hydraNodeCommand

-- | Convert an 'Options' instance into the corresponding list of command-line arguments.
--
-- This is useful in situations where one wants to programmatically define 'Options', providing
-- some measure of type safety, without having to juggle with strings.
toArgs :: RunOptions -> [String]
toArgs
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
    } =
    isVerbose verbosity
      <> ["--node-id", unpack nId]
      <> ["--listen", showHost listen]
      <> maybe [] (\h -> ["--advertise", showHost h]) advertise
      <> ["--api-host", show apiHost]
      <> toArgApiPort apiPort
      <> toWhichEtcd whichEtcd
      <> maybe [] (\cert -> ["--tls-cert", cert]) tlsCertPath
      <> maybe [] (\key -> ["--tls-key", key]) tlsKeyPath
      <> ["--hydra-signing-key", hydraSigningKey]
      <> concatMap (\vk -> ["--hydra-verification-key", vk]) hydraVerificationKeys
      <> concatMap toArgPeer peers
      <> maybe [] (\port -> ["--monitoring-port", show port]) monitoringPort
      <> ["--persistence-dir", persistenceDir]
      <> maybe [] (\rotateAfter -> ["--persistence-rotate-after", showPositive rotateAfter]) persistenceRotateAfter
      <> argsChainConfig chainConfig
      <> argsLedgerConfig
      <> ["--api-transaction-timeout", show apiTransactionTimeout]
   where
    (NodeId nId) = nodeId

    toWhichEtcd = \case
      SystemEtcd -> ["--use-system-etcd"]
      EmbeddedEtcd -> []

    isVerbose = \case
      Quiet -> ["--quiet"]
      _ -> []

    toArgPeer :: Host -> [String]
    toArgPeer p =
      ["--peer", show p]

    toArgStartChainFrom = \case
      Just ChainPointAtGenesis ->
        error "ChainPointAtGenesis"
      Just (ChainPoint (SlotNo slotNo) headerHash) ->
        let headerHashBase16 = toString (serialiseToRawBytesHexText headerHash)
         in ["--start-chain-from", show slotNo <> "." <> headerHashBase16]
      Nothing ->
        []

    argsChainConfig = \case
      Offline
        OfflineChainConfig
          { offlineHeadSeed
          , initialUTxOFile
          , ledgerGenesisFile
          } ->
          ["--offline-head-seed", toString $ serialiseToRawBytesHexText offlineHeadSeed]
            <> ["--initial-utxo", initialUTxOFile]
            <> case ledgerGenesisFile of
              Just fp -> ["--ledger-genesis", fp]
              Nothing -> []
      Cardano
        CardanoChainConfig
          { hydraScriptsTxId
          , cardanoSigningKey
          , cardanoVerificationKeys
          , startChainFrom
          , contestationPeriod
          , depositPeriod
          , unsyncedPeriod
          , chainBackendOptions
          } ->
          ( case chainBackendOptions of
              Blockfrost BlockfrostOptions{projectPath, queryTimeout, retryTimeout} ->
                ["--blockfrost", projectPath]
                  <> ["--blockfrost-query-timeout", show queryTimeout]
                  <> ["--blockfrost-retry-timeout", show retryTimeout]
              Direct DirectOptions{networkId, nodeSocket} ->
                toArgNetworkId networkId
                  <> toArgNodeSocket nodeSocket
          )
            <> ["--hydra-scripts-tx-id", intercalate "," $ toString . serialiseToRawBytesHexText <$> hydraScriptsTxId]
            <> ["--cardano-signing-key", cardanoSigningKey]
            <> ["--contestation-period", show contestationPeriod]
            <> ["--deposit-period", show depositPeriod]
            <> ["--unsynced-period", show unsyncedPeriod]
            <> concatMap (\vk -> ["--cardano-verification-key", vk]) cardanoVerificationKeys
            <> toArgStartChainFrom startChainFrom

    argsLedgerConfig =
      ["--ledger-protocol-parameters", cardanoLedgerProtocolParametersFile]

    CardanoLedgerConfig
      { cardanoLedgerProtocolParametersFile
      } = ledgerConfig

    showPositive :: Show a => Positive a -> String
    showPositive (Positive x) = show x

toArgNodeSocket :: SocketPath -> [String]
toArgNodeSocket nodeSocket = ["--node-socket", unFile nodeSocket]

toArgApiPort :: PortNumber -> [String]
toArgApiPort apiPort = ["--api-port", show apiPort]

toArgNetworkId :: NetworkId -> [String]
toArgNetworkId = \case
  Mainnet -> ["--mainnet"]
  Testnet (NetworkMagic magic) -> ["--testnet-magic", show magic]
