module Hydra.TUI.Options where

import Hydra.Prelude

import Hydra.Cardano.Api (NetworkId, SocketPath)
import Hydra.Network (Host (Host))
import Hydra.Options (networkIdParser)
import Hydra.Version (embeddedRevision, gitRevision, unknownVersion)
import Paths_hydra_tui (version)
import "base" Data.Version (Version (Version), showVersion)
import "optparse-applicative" Options.Applicative (
  Parser,
  auto,
  help,
  infoOption,
  long,
  metavar,
  option,
  short,
  showDefault,
  strOption,
  value,
 )

data Options = Options
  { hydraNodeHost :: Host
  , cardanoConnection :: Either FilePath SocketPath
  , cardanoNetworkId :: NetworkId
  , cardanoSigningKey :: FilePath
  -- ^ User key used by the tui client to commit
  }
  deriving stock (Eq, Show)

parseOptions :: Parser Options
parseOptions =
  ( Options
      <$> parseNodeHost
      <*> parsecardanoConnection
      <*> networkIdParser
      <*> parseCardanoSigningKey
  )
    <**> versionInfo
 where
  versionInfo :: Parser (Options -> Options)
  versionInfo =
    infoOption
      (showVersion ourVersion)
      (long "version" <> help "Show version")

  ourVersion =
    version & \(Version semver _) -> Version semver revision

  revision =
    maybeToList $
      embeddedRevision
        <|> gitRevision
        <|> Just unknownVersion

parsecardanoConnection :: Parser (Either FilePath SocketPath)
parsecardanoConnection = Right <$> cardanoNodeSocket <|> Left <$> blockFrost
 where
  cardanoNodeSocket :: Parser SocketPath
  cardanoNodeSocket =
    strOption
      ( long "node-socket"
          <> metavar "FILE"
          <> help "The path to the Cardano node domain socket for client communication."
          <> value "node.socket"
          <> showDefault
      )
  blockFrost :: Parser FilePath
  blockFrost =
    strOption
      ( long "blockfrost"
          <> metavar "FILE"
          <> help "The path to the Blockfrost project file."
          <> value "blockfrost-project.txt"
          <> showDefault
      )

parseNodeHost :: Parser Host
parseNodeHost =
  option
    auto
    ( long "connect"
        <> short 'c'
        <> help "Hydra-node to connect to in the form of <host>:<port>"
        <> value (Host "127.0.0.1" 4001)
        <> showDefault
    )

parseCardanoSigningKey :: Parser FilePath
parseCardanoSigningKey =
  strOption
    ( long "cardano-signing-key"
        <> short 'k'
        <> metavar "FILE"
        <> help "The path to the user signing key file used for selecting UTxO and signing a commit transaction. This file uses the same 'TextEnvelope' format as cardano-cli."
        <> value "me.sk"
        <> showDefault
    )
