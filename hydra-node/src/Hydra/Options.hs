module Hydra.Options (
  Options (..),
  ChainConfig (..),
  parseHydraOptions,
  parseHydraOptionsFromString,
  getParseResult,
  defaultOptions,
  ParserResult (..),
) where

import Data.IP (IP)
import Hydra.Chain.Direct (NetworkMagic (..))
import Hydra.Logging (Verbosity (..))
import Hydra.Network (Host, MockChain (..), PortNumber, defaultMockChain, readHost, readPort)
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
  , me :: FilePath
  , parties :: [FilePath]
  , chainConfig :: ChainConfig
  }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options (Verbose "HydraNode") 1 "127.0.0.1" 5001 [] "127.0.0.1" 4001 Nothing "me.sk" [] (MockChainConfig defaultMockChain)

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
    <*> signingKeyFileParser
    <*> many verificationKeyFileParser
    <*> chainConfigParser

data ChainConfig
  = MockChainConfig MockChain
  | DirectChainConfig
      { networkMagic :: NetworkMagic
      }
  deriving (Eq, Show)

chainConfigParser :: Parser ChainConfig
chainConfigParser = do
  mockChainConfigParser <|> directChainConfigParser
 where
  mockChainConfigParser =
    fmap
      MockChainConfig
      (makeMockChain <$> mockChainHostParser <*> mockChainPortsParser)
   where
    makeMockChain :: String -> (PortNumber, PortNumber, PortNumber) -> MockChain
    makeMockChain mockChainHost (syncPort, catchUpPort, postTxPort) =
      MockChain{mockChainHost, syncPort, catchUpPort, postTxPort}

  directChainConfigParser =
    DirectChainConfig
      <$> networkMagicParser

networkMagicParser :: Parser NetworkMagic
networkMagicParser =
  fmap NetworkMagic $
    option
      auto
      ( long "network-magic"
          <> metavar "MAGIC"
          <> value 42
          <> help "Network magic for the target network."
      )

signingKeyFileParser :: Parser FilePath
signingKeyFileParser =
  option
    str
    ( long "me"
        <> metavar "PATH"
        <> value "me.sk"
        <> help "A filepath to our signing key"
    )

verificationKeyFileParser :: Parser FilePath
verificationKeyFileParser =
  option
    str
    ( long "party"
        <> metavar "PATH"
        <> help "A verification key file of another party"
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

mockChainHostParser :: Parser String
mockChainHostParser = do
  strOption
    ( long "mock-chain-host"
        <> value "localhost"
        <> metavar "HOSTNAME"
        <> help "Address or hostname of the mock-chain (default: 'localhost')"
    )

mockChainPortsParser :: Parser (PortNumber, PortNumber, PortNumber)
mockChainPortsParser = do
  option
    auto
    ( long "mock-chain-ports"
        <> value (56789, 56790, 56791)
        <> metavar "[PORT]"
        <> help "The 3-tuple of ports to connect to mock-chain, in the order: sync, catch-up, post (default: (56789, 56790, 56791))"
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
