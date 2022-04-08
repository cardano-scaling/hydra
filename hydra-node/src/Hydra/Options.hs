module Hydra.Options (
  Options (..),
  ChainConfig (..),
  LedgerConfig (..),
  parseHydraOptions,
  parseHydraOptionsFromString,
  getParseResult,
  ParserResult (..),
  toArgs,
  defaultOptions,
  defaultLedgerConfig,
  defaultChainConfig,
) where

import Hydra.Prelude

import qualified Data.ByteString as BS
import Data.IP (IP (IPv4), toIPv4w)
import qualified Data.Text as T
import Hydra.Cardano.Api (
  ChainPoint (..),
  NetworkId (..),
  NetworkMagic (..),
  SlotNo (..),
  UsingRawBytesHex (..),
  deserialiseFromRawBytes,
  deserialiseFromRawBytesBase16,
  proxyToAsType,
  serialiseToRawBytesHexText,
 )
import Hydra.Logging (Verbosity (..))
import Hydra.Network (Host, PortNumber, readHost, readPort)
import Hydra.Node.Version (gitRevision, showFullVersion, version)
import Options.Applicative (
  Parser,
  ParserInfo,
  ParserResult (..),
  auto,
  completer,
  defaultPrefs,
  execParserPure,
  flag,
  fullDesc,
  getParseResult,
  handleParseResult,
  header,
  help,
  helper,
  info,
  infoOption,
  listCompleter,
  long,
  maybeReader,
  metavar,
  option,
  progDesc,
  short,
  strOption,
  value,
 )
import Options.Applicative.Builder (str)
import Test.QuickCheck (elements, listOf, listOf1, oneof, vectorOf)

data Options = Options
  { verbosity :: Verbosity
  , nodeId :: Natural
  , -- NOTE: Why not a 'Host'?
    host :: IP
  , port :: PortNumber
  , peers :: [Host]
  , apiHost :: IP
  , apiPort :: PortNumber
  , monitoringPort :: Maybe PortNumber
  , hydraSigningKey :: FilePath
  , hydraVerificationKeys :: [FilePath]
  , chainConfig :: ChainConfig
  , ledgerConfig :: LedgerConfig
  , -- TODO: Move into 'ChainConfig'
    startChainFrom :: Maybe ChainPoint
  }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions =
  Options
    { verbosity = Verbose "HydraNode"
    , nodeId = 1
    , host = "127.0.0.1"
    , port = 5001
    , peers = []
    , apiHost = "127.0.0.1"
    , apiPort = 4001
    , monitoringPort = Nothing
    , hydraSigningKey = "hydra.sk"
    , hydraVerificationKeys = []
    , chainConfig = defaultChainConfig
    , ledgerConfig = defaultLedgerConfig
    , startChainFrom = Nothing
    }

instance Arbitrary Options where
  arbitrary = do
    verbosity <- elements [Quiet, Verbose "HydraNode"]
    nodeId <- arbitrary
    host <- IPv4 . toIPv4w <$> arbitrary
    port <- arbitrary
    peers <- reasonablySized arbitrary
    apiHost <- IPv4 . toIPv4w <$> arbitrary
    apiPort <- arbitrary
    monitoringPort <- arbitrary
    hydraSigningKey <- genFilePath "sk"
    hydraVerificationKeys <- reasonablySized (listOf (genFilePath "vk"))
    chainConfig <- arbitrary
    ledgerConfig <- arbitrary
    startChainFrom <- oneof [pure Nothing, Just <$> genChainPoint]
    pure $
      defaultOptions
        { verbosity
        , nodeId
        , host
        , port
        , peers
        , apiHost
        , apiPort
        , monitoringPort
        , hydraSigningKey
        , hydraVerificationKeys
        , chainConfig
        , ledgerConfig
        , startChainFrom
        }

hydraNodeParser :: Parser Options
hydraNodeParser =
  Options
    <$> verbosityParser
    <*> nodeIdParser
    <*> hostParser
    <*> portParser
    <*> many peerParser
    <*> apiHostParser
    <*> apiPortParser
    <*> optional monitoringPortParser
    <*> hydraSigningKeyFileParser
    <*> many hydraVerificationKeyFileParser
    <*> chainConfigParser
    <*> ledgerConfigParser
    <*> optional startChainFromParser

data LedgerConfig = CardanoLedgerConfig
  { cardanoLedgerGenesisFile :: FilePath
  , cardanoLedgerProtocolParametersFile :: FilePath
  }
  deriving (Eq, Show)

defaultLedgerConfig :: LedgerConfig
defaultLedgerConfig =
  CardanoLedgerConfig
    { cardanoLedgerGenesisFile = "genesis-shelley.json"
    , cardanoLedgerProtocolParametersFile = "protocol-parameters.json"
    }

instance Arbitrary LedgerConfig where
  arbitrary = do
    cardanoLedgerGenesisFile <- genFilePath ".json"
    cardanoLedgerProtocolParametersFile <- genFilePath ".json"
    pure $ CardanoLedgerConfig{cardanoLedgerProtocolParametersFile, cardanoLedgerGenesisFile}

ledgerConfigParser :: Parser LedgerConfig
ledgerConfigParser =
  CardanoLedgerConfig
    <$> cardanoLedgerGenesisParser
    <*> cardanoLedgerProtocolParametersParser

cardanoLedgerGenesisParser :: Parser FilePath
cardanoLedgerGenesisParser =
  strOption
    ( long "ledger-genesis"
        <> metavar "FILE"
        <> value "genesis-shelley.json"
        <> help "Path to a Shelley-compatible genesis JSON file."
    )

cardanoLedgerProtocolParametersParser :: Parser FilePath
cardanoLedgerProtocolParametersParser =
  strOption
    ( long "ledger-protocol-parameters"
        <> metavar "FILE"
        <> value "protocol-parameters.json"
        <> help "Path to a JSON file describing protocol parameters (same format as returned from 'cardano-cli query protocol-parameters')"
    )

data ChainConfig = DirectChainConfig
  { networkId :: NetworkId
  , nodeSocket :: FilePath
  , cardanoSigningKey :: FilePath
  , cardanoVerificationKeys :: [FilePath]
  }
  deriving (Eq, Show)

defaultChainConfig :: ChainConfig
defaultChainConfig =
  DirectChainConfig
    { networkId = Testnet (NetworkMagic 42)
    , nodeSocket = "node.socket"
    , cardanoSigningKey = "cardano.sk"
    , cardanoVerificationKeys = []
    }

instance Arbitrary ChainConfig where
  arbitrary = do
    networkId <- Testnet . NetworkMagic <$> arbitrary
    nodeSocket <- genFilePath "socket"
    cardanoSigningKey <- genFilePath ".sk"
    cardanoVerificationKeys <- reasonablySized (listOf (genFilePath ".vk"))
    pure $
      DirectChainConfig
        { networkId
        , nodeSocket
        , cardanoSigningKey
        , cardanoVerificationKeys
        }

chainConfigParser :: Parser ChainConfig
chainConfigParser =
  DirectChainConfig
    <$> networkIdParser
    <*> nodeSocketParser
    <*> cardanoSigningKeyFileParser
    <*> many cardanoVerificationKeyFileParser

networkIdParser :: Parser NetworkId
networkIdParser =
  testnetParser
 where
  testnetParser =
    Testnet . NetworkMagic
      <$> option
        auto
        ( long "network-id"
            <> metavar "INTEGER"
            <> value 42
            <> completer (listCompleter ["1097911063", "42"])
            <> help "A test network with the given network magic."
        )

nodeSocketParser :: Parser FilePath
nodeSocketParser =
  strOption
    ( long "node-socket"
        <> metavar "FILE"
        <> value "node.socket"
        <> help "Local (Unix) socket path to connect to cardano node."
    )

cardanoSigningKeyFileParser :: Parser FilePath
cardanoSigningKeyFileParser =
  strOption
    ( long "cardano-signing-key"
        <> metavar "FILE"
        <> value "cardano.sk"
        <> help "Signing key for the internal wallet use for Chain interactions."
    )

cardanoVerificationKeyFileParser :: Parser FilePath
cardanoVerificationKeyFileParser =
  option
    str
    ( long "cardano-verification-key"
        <> metavar "FILE"
        <> help "Cardano verification key of other Hydra participant's wallet."
    )

hydraSigningKeyFileParser :: Parser FilePath
hydraSigningKeyFileParser =
  option
    str
    ( long "hydra-signing-key"
        <> metavar "FILE"
        <> value "hydra.sk"
        <> help "Our Hydra multisig signing key."
    )

hydraVerificationKeyFileParser :: Parser FilePath
hydraVerificationKeyFileParser =
  option
    str
    ( long "hydra-verification-key"
        <> metavar "FILE"
        <> help "Other party multisig verification key."
    )

peerParser :: Parser Host
peerParser =
  option
    (maybeReader readHost)
    ( long "peer"
        <> short 'P'
        <> help "A peer address in the form <host>:<port>, where <host> can be an IP address, or a host name"
    )

nodeIdParser :: Parser Natural
nodeIdParser =
  option
    auto
    ( long "node-id"
        <> short 'n'
        <> value 1
        <> metavar "INTEGER"
        <> help "Sets this node's id"
    )

verbosityParser :: Parser Verbosity
verbosityParser =
  flag
    (Verbose "HydraNode")
    Quiet
    ( long "quiet"
        <> short 'q'
        <> help "Turns off any logging"
    )

hostParser :: Parser IP
hostParser =
  option
    auto
    ( long "host"
        <> short 'h'
        <> value "127.0.0.1"
        <> metavar "IP"
        <> help "The address this node listens on for Hydra network peers connection (default: 127.0.0.1)"
    )

portParser :: Parser PortNumber
portParser =
  option
    (maybeReader readPort)
    ( long "port"
        <> short 'p'
        <> value 5001
        <> metavar "PORT"
        <> help "The port this node listens on for Hydra network peers connection (default: 5001)"
    )

apiHostParser :: Parser IP
apiHostParser =
  option
    auto
    ( long "api-host"
        <> value "127.0.0.1"
        <> metavar "IP"
        <> help "The address this node listens on for client API connections (default: 127.0.0.1)"
    )

apiPortParser :: Parser PortNumber
apiPortParser =
  option
    (maybeReader readPort)
    ( long "api-port"
        <> value 4001
        <> metavar "PORT"
        <> help "The port this node listens on for client API connections (default: 4001)"
    )

monitoringPortParser :: Parser PortNumber
monitoringPortParser =
  option
    (maybeReader readPort)
    ( long "monitoring-port"
        <> metavar "PORT"
        <> help "The port this node listens on for monitoring and metrics. If left empty, monitoring server is not started"
    )

startChainFromParser :: Parser ChainPoint
startChainFromParser =
  option
    (maybeReader readChainPoint)
    ( long "start-chain-from"
        <> metavar "SLOT.HEADER_HASH"
        <> help "The point at which to start on-chain component. Defaults to chain tip at startup time."
    )
 where
  readChainPoint :: String -> Maybe ChainPoint
  readChainPoint chainPointStr =
    case T.splitOn "." (toText chainPointStr) of
      [slotNoTxt, headerHashTxt] -> do
        slotNo <- SlotNo <$> readMaybe (toString slotNoTxt)
        UsingRawBytesHex headerHash <-
          either
            (const Nothing)
            Just
            (deserialiseFromRawBytesBase16 (encodeUtf8 headerHashTxt))
        pure $ ChainPoint slotNo headerHash
      _ ->
        Nothing

hydraNodeOptions :: ParserInfo Options
hydraNodeOptions =
  info
    (hydraNodeParser <**> helper <**> displayVersion)
    ( fullDesc
        <> progDesc "Starts a Hydra Node"
        <> header "hydra-node - A prototype of Hydra Head protocol"
    )
 where
  displayVersion =
    infoOption
      (showFullVersion version gitRevision)
      (long "version" <> help "Show version")

-- | Parse command-line arguments into a `Option` or exit with failure and error message.
parseHydraOptions :: IO Options
parseHydraOptions = getArgs <&> parseHydraOptionsFromString >>= handleParseResult

-- | Pure parsing of `Option` from a list of arguments.
parseHydraOptionsFromString :: [String] -> ParserResult Options
parseHydraOptionsFromString = execParserPure defaultPrefs hydraNodeOptions

-- | Convert an 'Options' instance into the corresponding list of command-line arguments.
--
-- This is useful in situations where one wants to programatically define 'Options', providing
-- some measure of type safety, without having to juggle with strings.
toArgs :: Options -> [String]
toArgs
  Options
    { verbosity
    , nodeId
    , host
    , port
    , peers
    , apiHost
    , apiPort
    , monitoringPort
    , hydraSigningKey
    , hydraVerificationKeys
    , chainConfig
    , ledgerConfig
    , startChainFrom
    } =
    isVerbose verbosity
      <> ["--node-id", show nodeId]
      <> ["--host", show host]
      <> ["--port", show port]
      <> ["--api-host", show apiHost]
      <> ["--api-port", show apiPort]
      <> ["--hydra-signing-key", hydraSigningKey]
      <> concatMap (\vk -> ["--hydra-verification-key", vk]) hydraVerificationKeys
      <> concatMap toArgPeer peers
      <> maybe [] (\mport -> ["--monitoring-port", show mport]) monitoringPort
      <> argsChainConfig
      <> argsLedgerConfig
      <> toArgStartChainFrom startChainFrom
   where
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

    toArgNetworkId = \case
      Mainnet -> error "Mainnet not supported"
      Testnet (NetworkMagic magic) -> show magic

    argsChainConfig =
      mempty
        <> ["--network-id", toArgNetworkId networkId]
        <> ["--node-socket", nodeSocket]
        <> ["--cardano-signing-key", cardanoSigningKey]
        <> concatMap (\vk -> ["--cardano-verification-key", vk]) cardanoVerificationKeys

    argsLedgerConfig =
      mempty
        <> ["--ledger-genesis", cardanoLedgerGenesisFile]
        <> ["--ledger-protocol-parameters", cardanoLedgerProtocolParametersFile]

    CardanoLedgerConfig
      { cardanoLedgerGenesisFile
      , cardanoLedgerProtocolParametersFile
      } = ledgerConfig

    DirectChainConfig
      { networkId
      , nodeSocket
      , cardanoSigningKey
      , cardanoVerificationKeys
      } = chainConfig

genFilePath :: String -> Gen FilePath
genFilePath extension = do
  path <- reasonablySized (listOf1 (elements ["a", "b", "c"]))
  pure $ intercalate "/" path <> "." <> extension

genChainPoint :: Gen ChainPoint
genChainPoint = ChainPoint <$> (SlotNo <$> arbitrary) <*> someHeaderHash
 where
  someHeaderHash = do
    bytes <- vectorOf 32 arbitrary
    let hash = fromMaybe (error "invalid bytes") $ deserialiseFromRawBytes (proxyToAsType Proxy) . BS.pack $ bytes
    pure hash
