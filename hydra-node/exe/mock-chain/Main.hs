{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Cardano.Prelude hiding (Option, option)
import Data.String (String)
import Data.Text (unpack)
import Hydra.Logging (Verbosity (Quiet, Verbose), withTracer)
import Hydra.MockZMQChain (
  catchUpTransactions,
  mockChainClient,
  runChainSync,
  startChain,
 )
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  execParser,
  flag,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  option,
  progDesc,
  short,
  strOption,
  value,
 )

data Option = Option
  { mode :: ChainMode
  , chainSyncAddress :: String
  , catchUpAddress :: String
  , postTxAddress :: String
  , -- TODO: provide less binary behaviour?
    verbosity :: Verbosity
  }
  deriving (Eq, Show)

data ChainMode = NodeMode | ClientMode | CatchUpMode
  deriving (Eq, Read, Show)

mockChainParser :: Parser Option
mockChainParser =
  Option
    <$> option
      auto
      ( long "mode"
          <> short 'm'
          <> value NodeMode
          <> help "Mode to run mock-chain, one of 'NodeMode', 'CatchUpMode' or 'ClientMode'"
      )
    <*> strOption
      ( long "sync-address"
          <> short 's'
          <> value "tcp://127.0.0.1:56789"
          <> help "The address where clients can connect for syncing transactions"
      )
    <*> strOption
      ( long "catch-up-address"
          <> short 'u'
          <> value "tcp://127.0.0.1:56790"
          <> help "The address where clients can connect for syncing transactions"
      )
    <*> strOption
      ( long "post-address"
          <> short 'p'
          <> value "tcp://127.0.0.1:56791"
          <> help "The address where clients can post transactions"
      )
    <*> flag
      (Verbose "MockChain")
      Quiet
      ( long "quiet"
          <> short 'q'
          <> help "Turns off any logging"
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
  Option{mode, chainSyncAddress, catchUpAddress, postTxAddress, verbosity} <- execParser mockChainOptions
  withTracer Nothing verbosity show $ \tracer -> case mode of
    NodeMode ->
      startChain chainSyncAddress catchUpAddress postTxAddress tracer
    CatchUpMode -> catchUpTransactions catchUpAddress print tracer
    ClientMode -> do
      async (runChainSync chainSyncAddress print tracer) >>= link
      forever $ do
        msg <- getLine
        case reads (unpack msg) of
          (tx, "") : _ -> liftIO $ mockChainClient postTxAddress tracer tx
          _ -> print $ "failed to read command: " <> msg
