{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Options (
  module Hydra.Options,
  ParserResult (..),
  renderFailure,
) where

import Hydra.Prelude

import Control.Arrow (left)
import Control.Lens ((?~))
import Data.Aeson (Value (Object, String), withObject, (.:))
import Data.Aeson.Lens (atKey)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IP (IP (IPv4), toIPv4, toIPv4w)
import Data.Text (unpack)
import Data.Text qualified as T
import Data.Version (Version (..), showVersion)
import Hydra.Cardano.Api (
  ChainPoint (..),
  File (..),
  NetworkId (..),
  NetworkMagic (..),
  SlotNo (..),
  SocketPath,
  TxId (..),
  deserialiseFromRawBytes,
  deserialiseFromRawBytesHex,
  proxyToAsType,
  serialiseToRawBytesHexText,
 )
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Contract qualified as Contract
import Hydra.Ledger.Cardano ()
import Hydra.Logging (Verbosity (..))
import Hydra.Network (Host (..), NodeId (NodeId), PortNumber, WhichEtcd (..), readHost, readPort, showHost)
import Hydra.Tx.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod), fromNominalDiffTime)
import Hydra.Tx.DepositDeadline (DepositDeadline (UnsafeDepositDeadline), depositFromNominalDiffTime)
import Hydra.Tx.HeadId (HeadSeed)
import Hydra.Version (embeddedRevision, gitRevision, unknownVersion)
import Options.Applicative (
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
import Options.Applicative.Builder (str)
import Options.Applicative.Help (vsep)
import Paths_hydra_node (version)
import Test.QuickCheck (elements, listOf, listOf1, oneof, suchThat, vectorOf)

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
  { chainBackend :: ChainBackend
  , publishSigningKey :: FilePath
  }
  deriving stock (Show, Eq)

-- | Default options as they should also be provided by 'runOptionsParser'.
defaultPublishOptions :: PublishOptions
defaultPublishOptions =
  PublishOptions
    { chainBackend = defaultDirectBackend
    , publishSigningKey = "cardano.sk"
    }

defaultDirectBackend :: ChainBackend
defaultDirectBackend =
  DirectBackend
    { publishNetworkId = Testnet (NetworkMagic 42)
    , publishNodeSocket = "node.socket"
    }

data ChainBackend
  = DirectBackend
      { publishNetworkId :: NetworkId
      , publishNodeSocket :: SocketPath
      }
  | BlockfrostBackend
      { projectPath :: FilePath
      }
  deriving stock (Show, Eq)

publishOptionsParser :: Parser PublishOptions
publishOptionsParser =
  PublishOptions <$> chainBackendParser <*> cardanoSigningKeyFileParser

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
  , chainConfig :: ChainConfig
  , ledgerConfig :: LedgerConfig
  , whichEtcd :: WhichEtcd
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Orphan instance
instance Arbitrary IP where
  arbitrary = IPv4 . toIPv4w <$> arbitrary
  shrink = genericShrink

instance Arbitrary RunOptions where
  arbitrary = do
    verbosity <- elements [Quiet, Verbose "HydraNode"]
    nodeId <- arbitrary
    listen <- arbitrary
    advertise <- arbitrary
    peers <- reasonablySized arbitrary
    apiHost <- arbitrary
    apiPort <- arbitrary
    tlsCertPath <- oneof [pure Nothing, Just <$> genFilePath "pem"]
    tlsKeyPath <- oneof [pure Nothing, Just <$> genFilePath "key"]
    monitoringPort <- arbitrary
    hydraSigningKey <- genFilePath "sk"
    hydraVerificationKeys <- reasonablySized (listOf (genFilePath "vk"))
    persistenceDir <- genDirPath
    chainConfig <- arbitrary
    ledgerConfig <- arbitrary
    whichEtcd <- arbitrary
    pure $
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
        , chainConfig
        , ledgerConfig
        , whichEtcd
        }

  shrink = genericShrink

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
    , chainConfig = Direct defaultDirectChainConfig
    , ledgerConfig = defaultLedgerConfig
    , whichEtcd = EmbeddedEtcd
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
    <*> chainConfigParser
    <*> ledgerConfigParser
    <*> whichEtcdParser

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
  Direct <$> directChainConfigParser
    <|> Offline <$> offlineChainConfigParser

chainBackendParser :: Parser ChainBackend
chainBackendParser = directBackendParser <|> blockfrostBackendParser
 where
  directBackendParser =
    DirectBackend
      <$> networkIdParser
      <*> nodeSocketParser

  blockfrostBackendParser =
    BlockfrostBackend
      <$> blockfrostProjectPathParser

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

instance Arbitrary LedgerConfig where
  arbitrary = do
    cardanoLedgerProtocolParametersFile <- genFilePath "json"
    pure $ CardanoLedgerConfig{cardanoLedgerProtocolParametersFile}

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
  | Direct DirectChainConfig
  deriving stock (Eq, Show, Generic)

instance ToJSON ChainConfig where
  toJSON = \case
    Offline cfg -> toJSON cfg & atKey "tag" ?~ String "OfflineChainConfig"
    Direct cfg -> toJSON cfg & atKey "tag" ?~ String "DirectChainConfig"

instance FromJSON ChainConfig where
  parseJSON =
    withObject "ChainConfig" $ \o ->
      o .: "tag" >>= \case
        "OfflineChainConfig" -> Offline <$> parseJSON (Object o)
        "DirectChainConfig" -> Direct <$> parseJSON (Object o)
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

data DirectChainConfig = DirectChainConfig
  { networkId :: NetworkId
  -- ^ Network identifer to which we expect to connect.
  , nodeSocket :: SocketPath
  -- ^ Path to a domain socket used to connect to the server.
  , hydraScriptsTxId :: [TxId]
  -- ^ Identifier of transaction holding the hydra scripts to use.
  , cardanoSigningKey :: FilePath
  -- ^ Path to the cardano signing key of the internal wallet.
  , cardanoVerificationKeys :: [FilePath]
  -- ^ Paths to other node's verification keys.
  , startChainFrom :: Maybe ChainPoint
  -- ^ Point at which to start following the chain.
  , contestationPeriod :: ContestationPeriod
  , depositDeadline :: DepositDeadline
  -- ^ Deadline to detect deposit tx on-chain.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

defaultDirectChainConfig :: DirectChainConfig
defaultDirectChainConfig =
  DirectChainConfig
    { networkId = Testnet (NetworkMagic 42)
    , nodeSocket = "node.socket"
    , hydraScriptsTxId = []
    , cardanoSigningKey = "cardano.sk"
    , cardanoVerificationKeys = []
    , startChainFrom = Nothing
    , contestationPeriod = defaultContestationPeriod
    , depositDeadline = defaultDepositDeadline
    }

instance Arbitrary ChainConfig where
  arbitrary =
    oneof
      [ Direct <$> genDirectChainConfig
      , Offline <$> genOfflineChainConfig
      ]
   where
    genDirectChainConfig = do
      networkId <- Testnet . NetworkMagic <$> arbitrary
      nodeSocket <- File <$> genFilePath "socket"
      hydraScriptsTxId <- arbitrary
      cardanoSigningKey <- genFilePath "sk"
      cardanoVerificationKeys <- reasonablySized (listOf (genFilePath "vk"))
      startChainFrom <- oneof [pure Nothing, Just <$> genChainPoint]
      contestationPeriod <- arbitrary `suchThat` (> UnsafeContestationPeriod 0)
      depositDeadline <- arbitrary `suchThat` (> UnsafeDepositDeadline 0)
      pure
        DirectChainConfig
          { networkId
          , nodeSocket
          , hydraScriptsTxId
          , cardanoSigningKey
          , cardanoVerificationKeys
          , startChainFrom
          , contestationPeriod
          , depositDeadline
          }

    genOfflineChainConfig = do
      offlineHeadSeed <- arbitrary
      ledgerGenesisFile <- oneof [pure Nothing, Just <$> genFilePath "json"]
      initialUTxOFile <- genFilePath "json"
      pure
        OfflineChainConfig
          { offlineHeadSeed
          , initialUTxOFile
          , ledgerGenesisFile
          }

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

directChainConfigParser :: Parser DirectChainConfig
directChainConfigParser =
  DirectChainConfig
    <$> networkIdParser
    <*> nodeSocketParser
    <*> (hydraScriptsTxIdsParser <|> many hydraScriptsTxIdParser)
    <*> cardanoSigningKeyFileParser
    <*> many cardanoVerificationKeyFileParser
    <*> optional startChainFromParser
    <*> contestationPeriodParser
    <*> depositDeadlineParser

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
        <> value defaultDirectChainConfig.nodeSocket
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
        <> value defaultDirectChainConfig.cardanoSigningKey
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

hydraNodeCommand :: ParserInfo Command
hydraNodeCommand =
  info
    ( commandParser
        <**> versionInfo
        <**> scriptInfo
        <**> helper
    )
    ( fullDesc
        <> progDesc "Starts a Hydra Node"
        <> header "hydra-node - Implementation of the Hydra Head protocol"
    )
 where
  versionInfo =
    infoOption
      (showVersion hydraNodeVersion)
      (long "version" <> help "Show version")

  scriptInfo =
    infoOption
      (decodeUtf8 $ encodePretty Contract.scriptInfo)
      (long "script-info" <> help "Dump script info as JSON")

hydraNodeVersion :: Version
hydraNodeVersion =
  version & \(Version semver _) -> Version semver revision
 where
  revision =
    maybeToList $
      embeddedRevision
        <|> gitRevision
        <|> Just unknownVersion

defaultContestationPeriod :: ContestationPeriod
defaultContestationPeriod = UnsafeContestationPeriod 600

defaultDepositDeadline :: DepositDeadline
defaultDepositDeadline = UnsafeDepositDeadline 3600

contestationPeriodParser :: Parser ContestationPeriod
contestationPeriodParser =
  option
    (parseNatural <|> parseViaDiffTime)
    ( long "contestation-period"
        <> metavar "SECONDS"
        <> value defaultContestationPeriod
        <> showDefault
        <> completer (listCompleter ["60", "180", "300"])
        <> help
          "Contestation period for close transaction in seconds. \
          \ If this value is not in sync with other participants hydra-node will ignore the initial tx.\
          \ Additionally, this value needs to make sense compared to the current network we are running."
    )
 where
  parseNatural = UnsafeContestationPeriod <$> auto

  parseViaDiffTime = auto >>= fromNominalDiffTime

depositDeadlineParser :: Parser DepositDeadline
depositDeadlineParser =
  option
    (parseNatural <|> parseViaDiffTime)
    ( long "deposit-deadline"
        <> metavar "SECONDS"
        <> value defaultDepositDeadline
        <> showDefault
        <> completer (listCompleter ["60", "180", "300"])
        <> help
          "Deadline for detecting the the deposit transaction on-chain expressed in seconds."
    )
 where
  parseNatural = UnsafeDepositDeadline <$> auto

  parseViaDiffTime = auto >>= depositFromNominalDiffTime

data InvalidOptions
  = MaximumNumberOfPartiesExceeded
  | CardanoAndHydraKeysMissmatch
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
    Direct DirectChainConfig{cardanoVerificationKeys}
      | max (length hydraVerificationKeys) (length cardanoVerificationKeys) + 1 > maximumNumberOfParties ->
          Left MaximumNumberOfPartiesExceeded
      | length cardanoVerificationKeys /= length hydraVerificationKeys ->
          Left CardanoAndHydraKeysMissmatch
      | otherwise -> Right ()

-- | Parse command-line arguments into a `Option` or exit with failure and error message.
parseHydraCommand :: IO Command
parseHydraCommand = getArgs <&> parseHydraCommandFromArgs >>= handleParseResult

-- | Pure parsing of `Option` from a list of arguments.
parseHydraCommandFromArgs :: [String] -> ParserResult Command
parseHydraCommandFromArgs = execParserPure defaultPrefs hydraNodeCommand

-- | Convert an 'Options' instance into the corresponding list of command-line arguments.
--
-- This is useful in situations where one wants to programatically define 'Options', providing
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
    , chainConfig
    , ledgerConfig
    , whichEtcd
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
      <> maybe [] (\mport -> ["--monitoring-port", show mport]) monitoringPort
      <> ["--persistence-dir", persistenceDir]
      <> argsChainConfig chainConfig
      <> argsLedgerConfig
   where
    (NodeId nId) = nodeId

    toWhichEtcd = \case
      SystemEtcd -> ["--use-system-etcd"]
      EmbeddedEtcd -> []

    isVerbose = \case
      Quiet -> ["--quiet"]
      _ -> []

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
      Direct
        DirectChainConfig
          { networkId
          , nodeSocket
          , hydraScriptsTxId
          , cardanoSigningKey
          , cardanoVerificationKeys
          , startChainFrom
          , contestationPeriod
          , depositDeadline
          } ->
          toArgNetworkId networkId
            <> toArgNodeSocket nodeSocket
            <> ["--hydra-scripts-tx-id", intercalate "," $ toString . serialiseToRawBytesHexText <$> hydraScriptsTxId]
            <> ["--cardano-signing-key", cardanoSigningKey]
            <> ["--contestation-period", show contestationPeriod]
            <> ["--deposit-deadline", show depositDeadline]
            <> concatMap (\vk -> ["--cardano-verification-key", vk]) cardanoVerificationKeys
            <> toArgStartChainFrom startChainFrom

    argsLedgerConfig =
      ["--ledger-protocol-parameters", cardanoLedgerProtocolParametersFile]

    CardanoLedgerConfig
      { cardanoLedgerProtocolParametersFile
      } = ledgerConfig

toArgNodeSocket :: SocketPath -> [String]
toArgNodeSocket nodeSocket = ["--node-socket", unFile nodeSocket]

toArgApiPort :: PortNumber -> [String]
toArgApiPort apiPort = ["--api-port", show apiPort]

toArgNetworkId :: NetworkId -> [String]
toArgNetworkId = \case
  Mainnet -> ["--mainnet"]
  Testnet (NetworkMagic magic) -> ["--testnet-magic", show magic]

genFilePath :: String -> Gen FilePath
genFilePath extension = do
  path <- reasonablySized (listOf1 (elements ["a", "b", "c"]))
  pure $ intercalate "/" path <> "." <> extension

genDirPath :: Gen FilePath
genDirPath = do
  path <- reasonablySized (listOf1 (elements ["a", "b", "c"]))
  pure $ intercalate "/" path

genChainPoint :: Gen ChainPoint
genChainPoint = (ChainPoint . SlotNo <$> arbitrary) <*> someHeaderHash
 where
  someHeaderHash = do
    bytes <- vectorOf 32 arbitrary
    let hash = either (error "invalid bytes") id $ deserialiseFromRawBytes (proxyToAsType Proxy) . BS.pack $ bytes
    pure hash
