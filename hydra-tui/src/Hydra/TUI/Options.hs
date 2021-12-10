module Hydra.TUI.Options where

import           Hydra.Prelude

import           Hydra.Ledger.Cardano (NetworkId (Mainnet, Testnet),
                                       NetworkMagic (NetworkMagic))
import           Hydra.Network        (Host (Host))
import           Options.Applicative  (Parser, auto, help, long, metavar,
                                       option, short, showDefault, strOption,
                                       value)

data Options = Options
  { hydraNodeHost          :: Host
  , cardanoNodeSocket      :: FilePath
  , cardanoNetworkId       :: NetworkId
  , cardanoVerificationKey :: FilePath
  }

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseNodeHost
    <*> parseCardanoNodeSocket
    <*> parseCardanoNetworkId
    <*> parseCardanoVerificationKey

parseCardanoNodeSocket :: Parser FilePath
parseCardanoNodeSocket =
  strOption
    ( long "cardano-node-socket"
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

parseCardanoNetworkId :: Parser NetworkId
parseCardanoNetworkId =
  ( Testnet . NetworkMagic
      <$> option
        auto
        ( long "network-magic"
            <> short 'n'
            <> metavar "INTEGER"
            <> help "The network magic number identifying the testnet to connect to."
            <> showDefault
        )
  )
    <|> pure Mainnet

parseCardanoVerificationKey :: Parser FilePath
parseCardanoVerificationKey =
  strOption
    ( long "cardano-verification-key"
        <> short 'k'
        <> metavar "FILE"
        <> help "The path to the verification key file used for selecting and committing UTxO. This file used the same 'Envelope' format than cardano-cli."
        <> value "me.vk"
        <> showDefault
    )
