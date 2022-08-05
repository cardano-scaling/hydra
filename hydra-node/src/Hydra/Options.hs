module Hydra.Options (
  module Hydra.Options,
  ParserResult (..),
) where

import Hydra.Prelude

import Control.Arrow (left)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IP (IP (IPv4), toIPv4w)
import qualified Data.Text as T
import Data.Version (showVersion)
import Hydra.Cardano.Api (
  AsType (AsTxId),
  ChainPoint (..),
  NetworkId (..),
  NetworkMagic (..),
  SlotNo (..),
  TxId,
  UsingRawBytesHex (..),
  deserialiseFromRawBytes,
  deserialiseFromRawBytesBase16,
  deserialiseFromRawBytesHex,
  proxyToAsType,
  serialiseToRawBytesHexText,
 )
import qualified Hydra.Contract as Contract
import Hydra.Ledger.Cardano ()
import Hydra.Logging (Verbosity (..))
import Hydra.Network (Host, PortNumber, readHost, readPort)
import Hydra.Node.Version (gitDescribe)
import Options.Applicative (
  Parser,
  ParserInfo,
  ParserResult (..),
  auto,
  completer,
  defaultPrefs,
  eitherReader,
  execParserPure,
  flag,
  fullDesc,
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
import Paths_hydra_node (version)
import Test.QuickCheck (elements, listOf, listOf1, oneof, vectorOf)

data Command
  = Run Options
  | Publish PublishOptions
  deriving (Show, Eq)

data PublishOptions
  = PublishOptions
  deriving (Show, Eq)

-- TODO: Rename to RunOptions for consistency
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
  , hydraScriptsTxId :: TxId
  , chainConfig :: ChainConfig
  , ledgerConfig :: LedgerConfig
  }
  deriving (Eq, Show)

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
    hydraScriptsTxId <- arbitrary
    chainConfig <- arbitrary
    ledgerConfig <- arbitrary
    pure $
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
        , hydraScriptsTxId
        , chainConfig
        , ledgerConfig
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
    <*> hydraScriptsTxIdParser
    <*> chainConfigParser
    <*> ledgerConfigParser

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
  , startChainFrom :: Maybe ChainPoint
  }
  deriving (Eq, Show)

defaultChainConfig :: ChainConfig
defaultChainConfig =
  DirectChainConfig
    { networkId = Testnet (NetworkMagic 42)
    , nodeSocket = "node.socket"
    , cardanoSigningKey = "cardano.sk"
    , cardanoVerificationKeys = []
    , startChainFrom = Nothing
    }

instance Arbitrary ChainConfig where
  arbitrary = do
    networkId <- Testnet . NetworkMagic <$> arbitrary
    nodeSocket <- genFilePath "socket"
    cardanoSigningKey <- genFilePath ".sk"
    cardanoVerificationKeys <- reasonablySized (listOf (genFilePath ".vk"))
    startChainFrom <- oneof [pure Nothing, Just <$> genChainPoint]
    pure $
      DirectChainConfig
        { networkId
        , nodeSocket
        , cardanoSigningKey
        , cardanoVerificationKeys
        , startChainFrom
        }

chainConfigParser :: Parser ChainConfig
chainConfigParser =
  DirectChainConfig
    <$> networkIdParser
    <*> nodeSocketParser
    <*> cardanoSigningKeyFileParser
    <*> many cardanoVerificationKeyFileParser
    <*> optional startChainFromParser

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

hydraScriptsTxIdParser :: Parser TxId
hydraScriptsTxIdParser =
  option
    (eitherReader $ left show . deserialiseFromRawBytesHex AsTxId . BSC.pack)
    ( long "hydra-scripts-tx-id"
        <> metavar "TXID"
        <> value "0101010101010101010101010101010101010101010101010101010101010101"
        <> help
          "The transaction which is expected to have published Hydra scripts as\
          \reference scripts in its outputs. Note: All scripts need to be in the\
          \first 10 outputs."
    )

hydraNodeCommand :: ParserInfo Command
hydraNodeCommand =
  info
    ( (Run <$> hydraNodeParser)
        <**> helper
        <**> versionInfo
        <**> scriptInfo
    )
    ( fullDesc
        <> progDesc "Starts a Hydra Node"
        <> header "hydra-node - A prototype of Hydra Head protocol"
    )
 where
  versionInfo =
    infoOption
      (fromMaybe (showVersion version) gitDescribe)
      (long "version" <> help "Show version")

  scriptInfo =
    infoOption
      (decodeUtf8 $ encodePretty Contract.scriptInfo)
      (long "script-info" <> help "Dump script info as JSON")

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
    , hydraScriptsTxId
    , chainConfig
    , ledgerConfig
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
      <> ["--hydra-scripts-tx-id", toString $ serialiseToRawBytesHexText hydraScriptsTxId]
      <> argsChainConfig
      <> argsLedgerConfig
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
      ["--network-id", toArgNetworkId networkId]
        <> ["--node-socket", nodeSocket]
        <> ["--cardano-signing-key", cardanoSigningKey]
        <> concatMap (\vk -> ["--cardano-verification-key", vk]) cardanoVerificationKeys
        <> toArgStartChainFrom startChainFrom

    argsLedgerConfig =
      ["--ledger-genesis", cardanoLedgerGenesisFile]
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
      , startChainFrom
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
