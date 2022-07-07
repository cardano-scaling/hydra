module Hydra.Cluster.Options where

import Hydra.Prelude
import Options.Applicative (Parser, flag', help, long, metavar, strOption)

data KnownNetwork
  = Testnet
  | VasilTestnet
  deriving (Show)

data Options = Options
  { knownNetwork :: KnownNetwork
  , faucetSigningKeyPath :: FilePath
  }
  deriving (Show)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseKnownNetwork
    <*> parseSigningKeyPath
 where
  parseKnownNetwork =
    flag' Testnet (long "testnet" <> help "The public testnet")
      <|> flag' VasilTestnet (long "vasil-dev" <> help "The latest vasil testnet")

  parseSigningKeyPath =
    strOption $
      long "faucet-signing-key-file"
        <> metavar "FILE"
        <> help "Filepath to a signing key holding ADA on the cardano network."
