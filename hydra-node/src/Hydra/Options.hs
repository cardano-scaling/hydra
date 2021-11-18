{-# LANGUAGE TypeApplications #-}

module Hydra.Options (
  Options (..),
  ChainConfig (..),
  parseHydraOptions,
  parseHydraOptionsFromString,
  getParseResult,
  ParserResult (..),
) where

import Data.IP (IP)
import Hydra.Chain.Direct (NetworkMagic (..))
import Hydra.Logging (Verbosity (..))
import Hydra.Network (Host, PortNumber, readHost, readPort)
import Hydra.Node.Version (gitRevision, showFullVersion, version)
import Hydra.Prelude
import Options.Applicative (
  Parser,
  ParserInfo,
  ParserResult (..),
  auto,
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

data Options = Options
  { verbosity :: Verbosity
  , nodeId :: Natural
  , host :: IP
  , port :: PortNumber
  , peers :: [Host]
  , apiHost :: IP
  , apiPort :: PortNumber
  , monitoringPort :: Maybe PortNumber
  , hydraSigningKey :: FilePath
  , hydraVerificationKeys :: [FilePath]
  , chainConfig :: ChainConfig
  }
  deriving (Eq, Show)

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

data ChainConfig = DirectChainConfig
  { networkMagic :: NetworkMagic
  , nodeSocket :: FilePath
  , cardanoSigningKey :: FilePath
  , cardanoVerificationKeys :: [FilePath]
  }
  deriving (Eq, Show)

chainConfigParser :: Parser ChainConfig
chainConfigParser =
  DirectChainConfig
    <$> networkMagicParser
    <*> nodeSocketParser
    <*> cardanoSigningKeyFileParser
    <*> many cardanoVerificationKeyFileParser

networkMagicParser :: Parser NetworkMagic
networkMagicParser =
  NetworkMagic
    <$> option
      auto
      ( long "network-magic"
          <> metavar "MAGIC"
          <> value 42
          <> help "Network magic for the target network."
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
