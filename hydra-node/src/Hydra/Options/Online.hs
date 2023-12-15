module Hydra.Options.Online (
  module Hydra.Options.Online
 ) where

import Hydra.Prelude
import Hydra.Cardano.Api (
  ChainPoint (..),
  SocketPath,
  NetworkId (Testnet, Mainnet),
  NetworkMagic (NetworkMagic),
  SlotNo (..),
  File (..),
  AsType (..),
  TxId(..),
  deserialiseFromRawBytesHex,
  HasTypeProxy (proxyToAsType), serialiseToRawBytesHexText,
 )
import Hydra.ContestationPeriod (ContestationPeriod (..))

import Hydra.Options.Common (
  cardanoVerificationKeyFileParser,
  genFilePath,
  defaultLedgerConfig,
  genChainPoint, LedgerConfig (..), verbosityParser, hostParser, portParser, apiHostParser, apiPortParser, monitoringPortParser, hydraSigningKeyFileParser, hydraVerificationKeyFileParser, persistenceDirParser, ledgerConfigParser, genDirPath, InvalidOptions (..),
 )

import Options.Applicative (
  Parser,
  auto,
  completer,
  eitherReader,
  flag',
  help,
  listCompleter,
  long,
  maybeReader,
  metavar,
  option,
  short,
  showDefault,
  strOption,
  value,
 )


import Hydra.Network (Host, NodeId (NodeId), readHost, PortNumber)
import Hydra.Ledger.Cardano ()
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Logging (Verbosity (..))

import Data.Text qualified as T
import Data.Text (unpack)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.IP (IP (IPv4), toIPv4)
import Options.Applicative.Builder (str)
import qualified Data.ByteString.Char8 as BSC
import Control.Arrow (left)

import Test.QuickCheck (elements, listOf, oneof, suchThat)

data ChainConfig = DirectChainConfig -- rename type constructor to directchainconfig
  { networkId :: NetworkId
  -- ^ Network identifer to which we expect to connect.
  , nodeSocket :: SocketPath
  -- ^ Path to a domain socket used to connect to the server.
  , cardanoSigningKey :: FilePath
  -- ^ Path to the cardano signing key of the internal wallet.
  , cardanoVerificationKeys :: [FilePath]
  -- ^ Paths to other node's verification keys.
  , startChainFrom :: Maybe ChainPoint
  -- ^ Point at which to start following the chain.
  , contestationPeriod :: ContestationPeriod
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
    nodeSocket <- File <$> genFilePath "socket"
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

nodeSocketParser :: Parser SocketPath
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
          \important to have a unique identifier in order to be able to \
          \distinguish between connected peers."
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

cardanoSigningKeyFileParser :: Parser FilePath
cardanoSigningKeyFileParser =
  strOption
    ( long "cardano-signing-key"
        <> metavar "FILE"
        <> showDefault
        <> value "cardano.sk"
        <> help
          "Cardano signing key of our hydra-node. This will be used to authorize \
          \Hydra protocol transactions for heads the node takes part in and any \
          \funds owned by this key will be used as 'fuel'."
    )

defaultContestationPeriod :: ContestationPeriod
defaultContestationPeriod = UnsafeContestationPeriod 60

contestationPeriodParser :: Parser ContestationPeriod
contestationPeriodParser =
  option
    (parseNatural <|> parseNominalDiffTime)
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

  parseNominalDiffTime =
    auto >>= \dt -> do
      let s = nominalDiffTimeToSeconds dt
      if s <= 0
        then fail "negative contestation period"
        else pure $ UnsafeContestationPeriod $ truncate s


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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
        <> ["--node-socket", unFile nodeSocket]
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

-- | Validate cmd line arguments for hydra-node and check if they make sense before actually running the node.
-- Rules we apply:
--  - Check if number of parties is bigger than our hardcoded limit
--      (by looking at loaded hydra or cardano keys and comparing it to the 'maximumNumberOfParties')
--  - Check if number of parties is more than zero when running in offline mode
--      (by looking at loaded hydra or cardano keys and checking if the offline config is set)
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


instance Arbitrary RunOptions where
  arbitrary = do
    verbosity <- elements [Quiet, Verbose "HydraNode"]
    nodeId <- arbitrary
    host <- arbitrary
    port <- arbitrary
    peers <- reasonablySized arbitrary
    apiHost <- arbitrary
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

  shrink = genericShrink


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

toArgNetworkId :: NetworkId -> [String]
toArgNetworkId = \case
  Mainnet -> ["--mainnet"]
  Testnet (NetworkMagic magic) -> ["--testnet-magic", show magic]
