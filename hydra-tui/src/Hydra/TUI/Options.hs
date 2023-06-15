{-# LANGUAGE TemplateHaskell #-}

module Hydra.TUI.Options where

import Hydra.Prelude

import Data.Version (Version (Version), showVersion)
import Hydra.Cardano.Api (NetworkId)
import Hydra.Network (Host (Host))
import Hydra.Options (networkIdParser)
import Hydra.Version (gitRevision)
import Language.Haskell.TH.Env (envQ)
import Options.Applicative (
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
import Paths_hydra_tui (version)

data Options = Options
  { hydraNodeHost :: Host
  , cardanoNodeSocket :: FilePath
  , cardanoNetworkId :: NetworkId
  , cardanoSigningKey :: FilePath
  }
  deriving stock (Eq, Show)

parseOptions :: Parser Options
parseOptions =
  ( Options
      <$> parseNodeHost
      <*> parseCardanoNodeSocket
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
      ($$(envQ "GIT_REVISION") :: Maybe String)
        <|> gitRevision

parseCardanoNodeSocket :: Parser FilePath
parseCardanoNodeSocket =
  strOption
    ( long "node-socket"
        <> metavar "FILE"
        <> help "The path to the Cardano node domain socket for client communication."
        <> value "node.socket"
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
        <> help "The path to the signing key file used for selecting, committing  UTxO and signing off-chain transactions. This file used the same 'Envelope' format than cardano-cli."
        <> value "me.sk"
        <> showDefault
    )
