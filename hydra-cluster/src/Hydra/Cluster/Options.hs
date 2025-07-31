{-# LANGUAGE OverloadedStrings #-}

module Hydra.Cluster.Options where

import Data.ByteString.Char8 qualified as BSC
import Data.List qualified as List
import Hydra.Cardano.Api (TxId, deserialiseFromRawBytesHex)
import Hydra.Cluster.Fixture (KnownNetwork (..))
import Hydra.Options (persistenceRotateAfterParser)
import Hydra.Prelude
import Options.Applicative (Parser, eitherReader, flag, flag', help, long, metavar, strOption)
import Options.Applicative.Builder (option)
import Test.QuickCheck (Positive)

data Options = Options
  { knownNetwork :: Maybe KnownNetwork
  , stateDirectory :: Maybe FilePath
  , publishHydraScripts :: PublishOrReuse
  , useMithril :: UseMithril
  , scenario :: Scenario
  , persistenceRotateAfter :: Maybe (Positive Natural)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data PublishOrReuse = Publish | Reuse [TxId]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data UseMithril = NotUseMithril | UseMithril
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data Scenario = Idle | RespendUTxO
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseKnownNetwork
    <*> parseStateDirectory
    <*> parsePublishHydraScripts
    <*> parseUseMithril
    <*> parseScenario
    <*> optional persistenceRotateAfterParser
 where
  parseKnownNetwork =
    flag' (Just Preview) (long "preview" <> help "The preview testnet")
      <|> flag' (Just Preproduction) (long "preprod" <> help "The pre-production testnet")
      <|> flag' (Just Mainnet) (long "mainnet" <> help "The mainnet")
      <|> flag' (Just Sanchonet) (long "sanchonet" <> help "The sanchonet preview testnet")
      <|> flag' (Just BlockfrostPreview) (long "blockfrost-preview" <> help "The preview testnet using Blockfrost")
      <|> flag' (Just BlockfrostPreprod) (long "blockfrost-preprod" <> help "The pre-production testnet using Blockfrost")
      <|> flag' (Just BlockfrostMainnet) (long "blockfrost-mainnet" <> help "The mainnet using Blockfrost")
      <|> flag'
        Nothing
        ( long "devnet"
            <> help
              "Create a local cardano devnet by running a cardano-node, start a\
              \hydra-node and open a single-party head in it. Generates a wallet\
              \key pair and commits some ADA into the head using it. The head is\
              \also simulating some traffic on this UTxO by re-spending it to\
              \the same key constantly. The keys are available on the\
              \state-directory. This is useful as a sandbox for development and\
              \testing."
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
        (eitherReader $ bimap show Reuse . parseTxIds)
        ( long "hydra-scripts-tx-id"
            <> metavar "TXID"
            <> help
              "Use the hydra scripts already published in given transaction id. \
              \See --publish-hydra-scripts or hydra-node publish-scripts"
        )
   where
    parseTxIds :: String -> Either String [TxId]
    parseTxIds str =
      let parsed = fmap (deserialiseFromRawBytesHex . BSC.pack) (List.lines str)
       in if null (lefts parsed) then Right (rights parsed) else Left ("Invalid TxId" :: String)

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

  parseScenario =
    flag
      Idle
      RespendUTxO
      ( long "busy"
          <> help "Start respending the same UTxO with a 100ms delay (only for devnet)."
      )
