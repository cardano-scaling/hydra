module Hydra.Cluster.Options where

import Data.ByteString.Char8 qualified as BSC
import Hydra.Cardano.Api (AsType (AsTxId), TxId, deserialiseFromRawBytesHex)
import Hydra.Cluster.Fixture (KnownNetwork (..))
import Hydra.Prelude
import Options.Applicative (Parser, eitherReader, flag, flag', help, long, metavar, strOption)
import Options.Applicative.Builder (option)

data Options = Options
  { knownNetwork :: Maybe KnownNetwork
  , stateDirectory :: Maybe FilePath
  , publishHydraScripts :: PublishOrReuse
  , useMithril :: UseMithril
  }
  deriving stock (Show)

data PublishOrReuse = Publish | Reuse TxId
  deriving stock (Show)

data UseMithril = NotUseMithril | UseMithril
  deriving stock (Show, Eq)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseKnownNetwork
    <*> parseStateDirectory
    <*> parsePublishHydraScripts
    <*> parseUseMithril
 where
  parseKnownNetwork =
    flag' (Just Preview) (long "preview" <> help "The preview testnet")
      <|> flag' (Just Preproduction) (long "preprod" <> help "The pre-production testnet")
      <|> flag' (Just Mainnet) (long "mainnet" <> help "The mainnet")
      <|> flag' (Just Sanchonet) (long "sanchonet" <> help "The sanchonet preview testnet")
      <|> flag'
        Nothing
        ( long "devnet"
            <> help
              ( toString $
                  unlines
                    [ "Create a local cardano devnet by running a cardano-node, "
                    , "start a hydra-node and open a single-party head in it. "
                    , "Generates a wallet key pair and commits some into the head using it. "
                    , "The keys are available on the state-directory. This is useful as a "
                    , "sandbox for development and testing."
                    ]
              )
        )

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
              "Use the hydra scripts already published in given transaction id. \
              \See --publish-hydra-scripts or hydra-node publish-scripts"
        )

  parseUseMithril =
    flag
      NotUseMithril
      UseMithril
      ( long "use-mithril"
          <> help
            "Use mithril-client to download and verify the latest network snapshot. \
            \When setting this, ensure that there is no db/ in --state-directory. \
            \If not set, the cardano-node will synchronize the network given the current \
            \cardano-node state in --state-directory."
      )
