module Hydra.Cluster.Options where

import qualified Data.ByteString.Char8 as BSC
import Hydra.Cardano.Api (AsType (AsTxId), TxId, deserialiseFromRawBytesHex)
import Hydra.Cluster.Fixture (KnownNetwork (..))
import Hydra.Prelude
import Options.Applicative (Parser, eitherReader, flag', help, long, metavar, strOption)
import Options.Applicative.Builder (option)

data Options = Options
  { knownNetwork :: KnownNetwork
  , stateDirectory :: Maybe FilePath
  , publishHydraScripts :: PublishOrReuse
  }
  deriving (Show)

data PublishOrReuse = Publish | Reuse TxId
  deriving (Show)

-- TODO: Provide an option to use mithril aggregated snapshots to bootstrap the testnet
parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseKnownNetwork
    <*> parseStateDirectory
    <*> parsePublishHydraScripts
 where
  parseKnownNetwork =
    flag' Testnet (long "testnet" <> help "The public testnet (soon EOL)")
      <|> flag' VasilDevnet (long "vasil-dev" <> help "The latest devnet used for testing vasil features")
      <|> flag' Preview (long "preview" <> help "The preview testnet")
      <|> flag' Preproduction (long "preprod" <> help "The pre-production testnet")

  parseStateDirectory =
    optional . strOption $
      long "state-directory"
        <> metavar "DIR"
        <> help
          "Filepath to the state directory used. If not given a temporary \
          \one is used. Note that this directory will contain the \
          \cardano-node state of the network and is potentially quite \
          \large (> 13GB for testnet)!"

  parsePublishHydraScripts =
    flag'
      Publish
      ( long "publish-hydra-scripts"
          <> help "Publish hydra scripts before running the scenario."
      )
      <|> option
        (eitherReader $ bimap show Reuse . deserialiseFromRawBytesHex AsTxId . BSC.pack)
        ( long "hydra-scripts-tx-id"
            <> metavar "TXID"
            <> help
              "Use the hydra scripts already published in given transaction id.\
              \See --publish-hydra-scripts or hydra-node publish-scripts"
        )
