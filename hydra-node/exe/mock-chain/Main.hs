{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Cardano.Prelude hiding (Option, async, option)
import Data.String
import Data.Text (unpack)
import Hydra.MockZMQChain
import Options.Applicative

data Option = Option
  { mode :: ChainMode
  , chainSyncAddress :: String
  , postTxAddress :: String
  }
  deriving (Eq, Show)

data ChainMode = NodeMode | ClientMode
  deriving (Eq, Show)

mockChainParser :: Parser Option
mockChainParser =
  Option
    <$> flag
      NodeMode
      ClientMode
      ( long "client"
          <> short 'c'
          <> help "Run in client mode, reading OnChainTx from the stdin"
      )
    <*> strOption
      ( long "sync-address"
          <> short 's'
          <> value "tcp://127.0.0.1:56789"
          <> help "The address where clients can connect for syncing transactions"
      )
    <*> strOption
      ( long "post-address"
          <> short 'p'
          <> value "tcp://127.0.0.1:56790"
          <> help "The address where clients can post transactions"
      )

mockChainOptions :: ParserInfo Option
mockChainOptions =
  info
    (mockChainParser <**> helper)
    ( fullDesc
        <> progDesc "Starts a mock Chain server"
        <> header "mock-chain - a mock chain server"
    )

main :: IO ()
main = do
  Option{mode, chainSyncAddress, postTxAddress} <- execParser mockChainOptions
  case mode of
    NodeMode -> startChain chainSyncAddress postTxAddress
    ClientMode -> do
      startChainSync chainSyncAddress print >>= link
      forever $ do
        msg <- getLine
        case reads (unpack msg) of
          (tx, "") : _ -> liftIO $ mockChainClient postTxAddress tx
          _ -> print $ "failed to read command: " <> msg
