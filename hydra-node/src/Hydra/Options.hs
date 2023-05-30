{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Options (
  module Hydra.Options,
  ParserResult (..),
) where

import Hydra.Prelude

import Control.Arrow (left)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IP (IP (IPv4), toIPv4, toIPv4w)
import Data.Text (unpack)
import qualified Data.Text as T
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Version (Version (..), showVersion)
import Hydra.Cardano.Api (
  AsType (AsTxId),
  ChainPoint (..),
  NetworkId (..),
  NetworkMagic (..),
  SlotNo (..),
  TxId (..),
  deserialiseFromRawBytes,
  deserialiseFromRawBytesHex,
  proxyToAsType,
  serialiseToRawBytesHexText,
 )
import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import qualified Hydra.Contract as Contract
import Hydra.Ledger.Cardano ()
import Hydra.Logging (Verbosity (..))
import Hydra.Network (Host, NodeId (NodeId), PortNumber, readHost, readPort)
import Hydra.Party (Party)
import Hydra.Version (gitRevision)
import Language.Haskell.TH.Env (envQ)
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
  info,
  infoOption,
  listCompleter,
  long,
  maybeReader,
  metavar,
  option,
  progDesc,
  progDescDoc,
  short,
  showDefault,
  strOption,
  subparser,
  value,
 )
import Options.Applicative.Builder (str)
import Options.Applicative.Help (vsep)
import Paths_hydra_node (version)
import Test.QuickCheck (elements, listOf, listOf1, oneof, suchThat, vectorOf)

-- | Hardcoded limit for maximum number of parties in a head protocol
-- The value is obtained from calculating the costs of running the scripts
-- and on-chan validators (see 'computeCollectComCost' 'computeAbortCost')
maximumNumberOfParties :: Int
maximumNumberOfParties = 4

data ParamMismatch
  = ContestationPeriodMismatch {loadedCp :: ContestationPeriod, configuredCp :: ContestationPeriod}
  | PartiesMismatch {loadedParties :: [Party], configuredParties :: [Party]}
  | StartChainFromNotAllowed
  deriving (Generic, Eq, Show, ToJSON)

instance Arbitrary ParamMismatch where
  arbitrary = genericArbitrary

data Command
  = Run RunOptions
  | Publish PublishOptions
  deriving (Show, Eq)

commandParser :: Parser Command
commandParser =
  asum
    [ Run <$> runOptionsParser
    , Publish <$> publishScriptsParser
    ]
 where
  publishScriptsParser :: Parser PublishOptions
  publishScriptsParser =
    subparser $
      command
        "publish-scripts"
        ( info
            (helper <*> publishOptionsParser)
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

data PublishOptions = PublishOptions
  { publishNetworkId :: NetworkId
  , publishNodeSocket :: FilePath
  , publishSigningKey :: FilePath
  }
  deriving (Show, Eq)

publishOptionsParser :: Parser PublishOptions
publishOptionsParser =
  PublishOptions
    <$> networkIdParser
    <*> nodeSocketParser
    <*> cardanoSigningKeyFileParser

data RunOptions = RunOptions
  { verbosity :: Verbosity
  , nodeId :: NodeId
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
  , persistenceDir :: FilePath
  , chainConfig :: ChainConfig
  , ledgerConfig :: LedgerConfig
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Arbitrary RunOptions where
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
    persistenceDir <- genDirPath
    chainConfig <- arbitrary
    ledgerConfig <- arbitrary
    pure $
      RunOptions
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
        , persistenceDir
        , chainConfig
        , ledgerConfig
        }

runOptionsParser :: Parser RunOptions
runOptionsParser =
  RunOptions
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
    <*> persistenceDirParser
    <*> chainConfigParser
    <*> ledgerConfigParser

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
    cardanoLedgerProtocolParametersFile <- genFilePath ".json"
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

data ChainConfig = DirectChainConfig
  { networkId :: NetworkId
  -- ^ Network identifer to which we expect to connect.
  , nodeSocket :: FilePath
  -- ^ Path to a domain socket used to connect to the server.
  , cardanoSigningKey :: FilePath
  -- ^ Path to the cardano signing key of the internal wallet.
  , cardanoVerificationKeys :: [FilePath]
  -- ^ Paths to other node's verification keys.
  , startChainFrom :: Maybe ChainPoint
  -- ^ Point at which to start following the chain.
  , contestationPeriod :: ContestationPeriod
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

defaultChainConfig :: ChainConfig
defaultChainConfig =
  DirectChainConfig
    { networkId = Testnet (NetworkMagic 42)
    , nodeSocket = "node.socket"
    , cardanoSigningKey = "cardano.sk"
    , cardanoVerificationKeys = []
    , startChainFrom = Nothing
    , contestationPeriod = defaultContestationPeriod
    }

instance Arbitrary ChainConfig where
  arbitrary = do
    networkId <- Testnet . NetworkMagic <$> arbitrary
    nodeSocket <- genFilePath "socket"
    cardanoSigningKey <- genFilePath ".sk"
    cardanoVerificationKeys <- reasonablySized (listOf (genFilePath ".vk"))
    startChainFrom <- oneof [pure Nothing, Just <$> genChainPoint]
    contestationPeriod <- arbitrary `suchThat` (> UnsafeContestationPeriod 0)
    pure $
      DirectChainConfig
        { networkId
        , nodeSocket
        , cardanoSigningKey
        , cardanoVerificationKeys
        , startChainFrom
        , contestationPeriod
        }

chainConfigParser :: Parser ChainConfig
chainConfigParser =
  DirectChainConfig
    <$> networkIdParser
    <*> nodeSocketParser
    <*> cardanoSigningKeyFileParser
    <*> many cardanoVerificationKeyFileParser
    <*> optional startChainFromParser
    <*> contestationPeriodParser

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

nodeSocketParser :: Parser FilePath
nodeSocketParser =
  strOption
    ( long "node-socket"
        <> metavar "FILE"
        <> value "node.socket"
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
        <> value "cardano.sk"
        <> help
          "Cardano signing key of our hydra-node. This will be used to 'fuel' \
          \and sign Hydra protocol transactions, as well as commit UTxOs from."
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
        <> help
          "The Hydra node identifier used on the Hydra network. It is \
          \important to have a unique identifier in order to be able \
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

hostParser :: Parser IP
hostParser =
  option
    auto
    ( long "host"
        <> short 'h'
        -- XXX: This is default does not make sense, should use 0.0.0.0.
        <> value "127.0.0.1"
        <> showDefault
        <> metavar "IP"
        <> help "Listen address for incoming Hydra network connections."
    )

portParser :: Parser PortNumber
portParser =
  option
    (maybeReader readPort)
    ( long "port"
        <> short 'p'
        <> value 5001
        <> showDefault
        <> metavar "PORT"
        <> help "Listen port for incoming Hydra network connections."
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
          "The id of the block we want to start observing the chain from. \
          \If not given, uses the chain tip at startup. Composed by the slot \
          \number, a separator ('.') and the hash of the block header. \
          \For example: 52970883.d36a9936ae7a07f5f4bdc9ad0b23761cb7b14f35007e54947e27a1510f897f04."
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
              deserialiseFromRawBytesHex (proxyToAsType Proxy) (encodeUtf8 headerHashTxt)
          pure $ ChainPoint slotNo headerHash
        _emptyOrSingularList ->
          Nothing

hydraScriptsTxIdParser :: Parser TxId
hydraScriptsTxIdParser =
  option
    (eitherReader $ left show . deserialiseFromRawBytesHex AsTxId . BSC.pack)
    ( long "hydra-scripts-tx-id"
        <> metavar "TXID"
        <> value "0101010101010101010101010101010101010101010101010101010101010101"
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
        <> header "hydra-node - A prototype of Hydra Head protocol"
    )
 where
  versionInfo =
    infoOption
      (showVersion ourVersion)
      (long "version" <> help "Show version")

  ourVersion =
    version & \(Version semver _) -> Version semver revision

  revision =
    maybeToList $
      ($$(envQ "GIT_REVISION") :: Maybe String)
        <|> gitRevision

  scriptInfo =
    infoOption
      (decodeUtf8 $ encodePretty Contract.scriptInfo)
      (long "script-info" <> help "Dump script info as JSON")

defaultContestationPeriod :: ContestationPeriod
defaultContestationPeriod = UnsafeContestationPeriod 60

contestationPeriodParser :: Parser ContestationPeriod
contestationPeriodParser =
  option
    (parseNatural <|> parseNominalDiffTime)
    ( long "contestation-period"
        <> metavar "CONTESTATION-PERIOD"
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

  parseNominalDiffTime =
    auto >>= \dt -> do
      let s = nominalDiffTimeToSeconds dt
      if s <= 0
        then fail "negative contestation period"
        else pure $ UnsafeContestationPeriod $ truncate s

data InvalidOptions
  = MaximumNumberOfPartiesExceeded
  | CardanoAndHydraKeysMissmatch
  deriving (Eq, Show)

explain :: InvalidOptions -> String
explain = \case
  MaximumNumberOfPartiesExceeded -> "Maximum number of parties is currently set to: " <> show maximumNumberOfParties
  CardanoAndHydraKeysMissmatch -> "Number of loaded cardano and hydra keys needs to match"

-- | Validate cmd line arguments for hydra-node and check if they make sense before actually running the node.
-- Rules we apply:
--  - Check if number of parties is bigger than our hardcoded limit
--      (by looking at loaded hydra or cardano keys and comparing it to the 'maximumNumberOfParties')
--  - Check that number of loaded hydra keys match with the number of loaded cardano keys
--      (by comparing lengths of the two lists)
validateRunOptions :: RunOptions -> Either InvalidOptions ()
validateRunOptions RunOptions{hydraVerificationKeys, chainConfig}
  | numberOfOtherParties + 1 > maximumNumberOfParties = Left MaximumNumberOfPartiesExceeded
  | length (cardanoVerificationKeys chainConfig) /= length hydraVerificationKeys =
      Left CardanoAndHydraKeysMissmatch
  | otherwise = Right ()
 where
  -- let's take the higher number of loaded cardano/hydra keys
  numberOfOtherParties =
    max (length hydraVerificationKeys) (length $ cardanoVerificationKeys chainConfig)

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
    , host
    , port
    , peers
    , apiHost
    , apiPort
    , monitoringPort
    , hydraSigningKey
    , hydraVerificationKeys
    , hydraScriptsTxId
    , persistenceDir
    , chainConfig
    , ledgerConfig
    } =
    isVerbose verbosity
      <> ["--node-id", unpack nId]
      <> ["--host", show host]
      <> ["--port", show port]
      <> ["--api-host", show apiHost]
      <> ["--api-port", show apiPort]
      <> ["--hydra-signing-key", hydraSigningKey]
      <> concatMap (\vk -> ["--hydra-verification-key", vk]) hydraVerificationKeys
      <> concatMap toArgPeer peers
      <> maybe [] (\mport -> ["--monitoring-port", show mport]) monitoringPort
      <> ["--hydra-scripts-tx-id", toString $ serialiseToRawBytesHexText hydraScriptsTxId]
      <> ["--persistence-dir", persistenceDir]
      <> argsChainConfig
      <> argsLedgerConfig
   where
    (NodeId nId) = nodeId
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

    argsChainConfig =
      toArgNetworkId networkId
        <> ["--node-socket", nodeSocket]
        <> ["--cardano-signing-key", cardanoSigningKey]
        <> ["--contestation-period", show contestationPeriod]
        <> concatMap (\vk -> ["--cardano-verification-key", vk]) cardanoVerificationKeys
        <> toArgStartChainFrom startChainFrom

    argsLedgerConfig =
      ["--ledger-protocol-parameters", cardanoLedgerProtocolParametersFile]

    CardanoLedgerConfig
      { cardanoLedgerProtocolParametersFile
      } = ledgerConfig

    DirectChainConfig
      { networkId
      , nodeSocket
      , cardanoSigningKey
      , cardanoVerificationKeys
      , startChainFrom
      , contestationPeriod
      } = chainConfig

defaultRunOptions :: RunOptions
defaultRunOptions =
  RunOptions
    { verbosity = Verbose "HydraNode"
    , nodeId = NodeId "hydra-node-1"
    , host = localhost
    , port = 5001
    , peers = []
    , apiHost = localhost
    , apiPort = 4001
    , monitoringPort = Nothing
    , hydraSigningKey = "hydra.sk"
    , hydraVerificationKeys = []
    , hydraScriptsTxId = TxId "0101010101010101010101010101010101010101010101010101010101010101"
    , persistenceDir = "./"
    , chainConfig = defaultChainConfig
    , ledgerConfig = defaultLedgerConfig
    }
 where
  localhost = IPv4 $ toIPv4 [127, 0, 0, 1]

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
genChainPoint = ChainPoint <$> (SlotNo <$> arbitrary) <*> someHeaderHash
 where
  someHeaderHash = do
    bytes <- vectorOf 32 arbitrary
    let hash = either (error "invalid bytes") id $ deserialiseFromRawBytes (proxyToAsType Proxy) . BS.pack $ bytes
    pure hash
