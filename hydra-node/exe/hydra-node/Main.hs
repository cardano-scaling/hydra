module Main where

import Cardano.Prelude hiding (option)

import Data.Text (pack, unpack)
import Hydra.Ledger.MockTx (mockLedger)
import Hydra.Logic (Party (Party))
import Hydra.MockRepl (startHydraRepl)
import Hydra.MockZMQChain (mockChainClient, startChainSync)
import Hydra.Node (HydraNode (oc), OnChain (..), createHydraNode, handleChainTx, runHydraNode)
import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, header, help, helper, info, long, metavar, option, progDesc, short, strOption, value)

data Options = Options
  { chainSyncAddress :: Text
  , postTxAddress :: Text
  , nodeId :: Party
  }

hydraNodeOptions :: ParserInfo Options
hydraNodeOptions =
  info
    (hydraNodeOptionsParser <**> helper)
    ( fullDesc
        <> progDesc "Starts a Hydra node"
        <> header "hydra-node - a single Hydra node"
    )

hydraNodeOptionsParser :: Parser Options
hydraNodeOptionsParser =
  Options
    <$> chainSyncAddressParser
    <*> postTxAddressParser
    <*> partyParser
 where
  chainSyncAddressParser =
    ( pack
        <$> strOption
          ( long "sync-address"
              <> short 's'
              <> value "tcp://127.0.0.1:56789"
              <> help "The address to connect to the chain for syncing (receiving) transactions"
          )
    )
  postTxAddressParser =
    ( pack
        <$> strOption
          ( long "post-address"
              <> short 'p'
              <> value "tcp://127.0.0.1:56790"
              <> help "The address to connect to the chain for posting new transactions"
          )
    )
  partyParser :: Parser Party
  partyParser =
    Party . fromInteger
      <$> option
        auto
        ( long "node-id"
            <> short 'n'
            <> metavar "INT"
            <> help "This Hydra node identifier (an integer)"
        )

main :: IO ()
main = do
  Options{chainSyncAddress, postTxAddress, nodeId} <- execParser hydraNodeOptions
  node <- createHydraNode nodeId mockLedger
  let mockChainNode = node{oc = OnChain $ mockChainClient (unpack postTxAddress)}
  startHydraRepl mockChainNode
  startChainSync (unpack chainSyncAddress) (handleChainTx mockChainNode) >>= link
  runHydraNode mockChainNode
