module Hydra.Option (
  Option (..),
  parseHydraOptions,
  parseHydraOptionsFromString,
  getParseResult,
  defaultOption,
) where

import Cardano.Prelude hiding (Option, option)
import Data.IP (IP)
import Data.String (String)
import Hydra.Logging (Verbosity (..))
import Hydra.Network (Host, PortNumber, readHost, readPort)
import Options.Applicative (
  Parser,
  ParserInfo,
  ParserResult,
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
  long,
  maybeReader,
  metavar,
  option,
  progDesc,
  short,
  switch,
  value,
 )

data Option = Option
  { verbosity :: Verbosity
  , nodeId :: Natural
  , host :: IP
  , port :: PortNumber
  , peers :: [Host]
  , apiHost :: IP
  , apiPort :: PortNumber
  , monitoringPort :: Maybe PortNumber
  , displayVersion :: Bool
  }
  deriving (Eq, Show)

defaultOption :: Option
defaultOption = Option (Verbose "HydraNode") 1 "127.0.0.1" 5001 [] "127.0.0.1" 4001 Nothing False

hydraNodeParser :: Parser Option
hydraNodeParser =
  Option
    <$> verbosityParser
    <*> nodeIdParser
    <*> hostParser
    <*> portParser
    <*> many peerParser
    <*> apiHostParser
    <*> apiPortParser
    <*> optional monitoringPortParser
    <*> displayVersionParser

peerParser :: Parser Host
peerParser =
  option
    (maybeReader readHost)
    ( long "peer"
        <> short 'P'
        <> help "A peer address in the form <host>@<port>, where <host> can be an IP address, or a host name"
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

displayVersionParser :: Parser Bool
displayVersionParser =
  switch
    ( long "version"
        <> help "Show version number and exit"
    )

hydraNodeOptions :: ParserInfo Option
hydraNodeOptions =
  info
    (hydraNodeParser <**> helper)
    ( fullDesc
        <> progDesc "Starts a Hydra Node"
        <> header "hydra-node - A prototype of Hydra Head protocol"
    )

-- | Parse command-line arguments into a `Option` or exit with failure and error message.
parseHydraOptions :: IO Option
parseHydraOptions = getArgs <&> parseHydraOptionsFromString >>= handleParseResult

-- | Pure parsing of `Option` from a list of arguments.
parseHydraOptionsFromString :: [String] -> ParserResult Option
parseHydraOptionsFromString = execParserPure defaultPrefs hydraNodeOptions
