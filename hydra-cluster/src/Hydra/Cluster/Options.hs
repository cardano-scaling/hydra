module Hydra.Cluster.Options where

import Hydra.Cluster.Fixture (KnownNetwork (..))
import Hydra.Prelude
import Options.Applicative (Parser, flag', help, long, metavar, strOption)

data Options = Options
  { knownNetwork :: KnownNetwork
  , stateDirectory :: Maybe FilePath
  }
  deriving (Show)

-- TODO: Provide an option to use mithril aggregated snapshots to bootstrap the testnet
parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseKnownNetwork
    <*> parseStateDirectory
 where
  parseKnownNetwork =
    flag' Testnet (long "testnet" <> help "The public testnet")
      <|> flag' VasilTestnet (long "vasil-dev" <> help "The latest vasil testnet")

  parseStateDirectory =
    optional . strOption $
      long "state-directory"
        <> metavar "DIR"
        <> help
          "Filepath to the state directory used. If not given a temporary \
          \one is used. Note that this directory will contain the \
          \cardano-node state of the network and is potentially quite \
          \large (> 13GB for testnet)!"
