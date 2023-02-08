module Hydra.TUI.Options where

import Hydra.Prelude

import Hydra.Cardano.Api (
  NetworkId (Mainnet, Testnet),
  NetworkMagic (NetworkMagic),
 )
import Hydra.Network (Host (Host))
import Options.Applicative (
  Parser,
  ReadM,
  auto,
  completer,
  eitherReader,
  help,
  listCompleter,
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
  , cardanoNodeSocket :: FilePath
  , cardanoNetworkId :: NetworkId
  , cardanoSigningKey :: FilePath
  }

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseNodeHost
    <*> parseCardanoNodeSocket
    <*> parseCardanoNetworkId
    <*> parseCardanoSigningKey

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
        <> value (Host "0.0.0.0" 4001)
        <> showDefault
    )

-- TODO: this is now the same parser as is Hydra.Options. DRY?
parseCardanoNetworkId :: Parser NetworkId
parseCardanoNetworkId =
  option
    (parseTestnet <|> parseMainnet)
    ( long "network-id"
        <> short 'n'
        <> showDefault
        <> completer (listCompleter ["1097911063", "42", "Mainnet", "mainnet", "m"])
        <> help
          "Either Mainnet network or a magic number identifying the testnet to connect to."
    )
 where
  parseTestnet = eitherReader go
   where
    go :: String -> Either String NetworkId
    go s =
      case readMaybe s :: Maybe Word32 of
        Nothing -> Left "Could not parse Testnet network magic"
        Just i -> pure $ Testnet (NetworkMagic i)

  parseMainnet = eitherReader go
   where
    go :: String -> Either String NetworkId
    go "Mainnet" = pure Mainnet
    go "mainnet" = pure Mainnet
    go "m" = pure Mainnet
    go _ = Left "Expected TEXT value (Mainnet, mainnet or just m)"

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
