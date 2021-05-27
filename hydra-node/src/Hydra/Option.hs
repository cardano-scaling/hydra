module Hydra.Option where

import Cardano.Prelude hiding (Option, option)
import Data.IP (IP)
import Hydra.Logging (Verbosity (..))
import Hydra.Network (Port)
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  execParser,
  flag,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  strOption,
  value,
 )

data Option = Option
  { verbosity :: Verbosity
  , nodeId :: Natural
  , host :: IP
  , port :: Port
  }
  deriving (Show)

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
  strOption
    ( long "port"
        <> short 'p'
        <> value "5001"
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

parseHydraOptions :: IO Option
parseHydraOptions = execParser hydraNodeOptions
