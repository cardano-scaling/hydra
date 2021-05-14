{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Cardano.Prelude hiding (Option, async, option)
import Data.String
import Hydra.MockZMQChain
import Options.Applicative

data Option = Option
  { chainSyncAddress :: String
  , postTxAddress :: String
  }
  deriving (Eq, Show)

mockChainParser :: Parser Option
mockChainParser =
  Option
    <$> strOption
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
  Option{chainSyncAddress, postTxAddress} <- execParser mockChainOptions
  startChain chainSyncAddress postTxAddress
