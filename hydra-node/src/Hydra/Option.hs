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
import Hydra.Network (Port, readPort)
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
  value,
 )

data Option = Option
  { verbosity :: Verbosity
  , nodeId :: Natural
  , host :: IP
  , port :: Port
  }
  deriving (Eq, Show)

defaultOption :: Option
defaultOption = Option (Verbose "HydraNode") 1 "127.0.0.1" 5001

hydraNodeParser :: Parser Option
hydraNodeParser =
  Option
    <$> verbosityParser
    <*> nodeIdParser
    <*> hostParser
    <*> portParser

nodeIdParser :: Parser Natural
nodeIdParser =
  option
    auto
    ( long "node-id"
        <> short 'n'
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
        <> help "The address this node listens on (default: 127.0.0.1)"
    )

portParser :: Parser Port
portParser =
  option
    (maybeReader readPort)
    ( long "port"
        <> short 'p'
        <> value 5001
        <> metavar "PORT"
        <> help "The port this node listens on (default: 5001)"
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
