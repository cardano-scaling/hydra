{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.STM (
  TChan,
  dupTChan,
  newBroadcastTChanIO,
  writeTChan,
 )
import Hydra.Cardano.Api (Tx)
import Hydra.Logging (Verbosity (..), withTracer)
import Hydra.Network (Host (..))
import Hydra.Network.Message (Message (ReqSn))
import Hydra.Network.Ouroboros (TraceOuroborosNetwork, WithHost, connectToPeers, hydraClient, withIOManager)
import Hydra.Options (hydraVerificationKeyFileParser, peerParser)
import Hydra.Prelude
import Hydra.Snapshot (SnapshotNumber (UnsafeSnapshotNumber))
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  command,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
 )

data Options = InjectReqSn
  { peer :: Host
  -- ^ The host to connect to
  , snapshotNumber :: SnapshotNumber
  -- ^ The number of the snapshot to inject
  , hydraKey :: FilePath
  -- ^ The verification key to impersonate
  }
  deriving stock (Show)

injectReqSnParser :: Parser Options
injectReqSnParser =
  InjectReqSn
    <$> peerParser
    <*> snapshotNumberParser
    <*> hydraVerificationKeyFileParser

snapshotNumberParser :: Parser SnapshotNumber
snapshotNumberParser =
  UnsafeSnapshotNumber
    <$> option
      auto
      ( long "snapshot-number"
          <> short 's'
          <> metavar "NATURAL"
          <> help
            "The number of the snapshot to craft a ReqSn for"
      )

commandsParser :: Parser Options
commandsParser =
  hsubparser
    ( command
        "reqsn"
        ( info
            (helper <*> injectReqSnParser)
            (progDesc "Inject a ReqSn message for given number seemingly from another peer.")
        )
    )

netOptions :: ParserInfo Options
netOptions =
  info
    ( commandsParser
        <**> helper
    )
    ( fullDesc
        <> progDesc "Hydra Network Injector"
        <> header "hydra-net - CLI tool to inject messages into a Hydra nodes network"
    )

main :: IO ()
main =
  execParser netOptions >>= \case
    InjectReqSn{peer, snapshotNumber, hydraKey} -> injectReqSn peer snapshotNumber hydraKey

injectReqSn :: Host -> SnapshotNumber -> FilePath -> IO ()
injectReqSn peer snapshotNumber _hydraKeyFile = do
  let localHost = Host "127.0.0.1" 12345
  --  vk <- readFileTextEnvelopeThrow (AsVerificationKey AsHydraKey) hydraKeyFile
  withIOManager $ \iomgr -> do
    bchan <- newBroadcastTChanIO
    let newBroadcastChannel = atomically $ dupTChan bchan
    withTracer @_ @(WithHost (TraceOuroborosNetwork (Message Tx))) (Verbose "hydra-net") $ \tracer ->
      concurrently_
        (connectToPeers tracer localHost [peer] iomgr newBroadcastChannel hydraClient)
        (sendReqSn bchan)
 where
  sendReqSn :: TChan (Message Tx) -> IO ()
  sendReqSn chan = do
    let msg = ReqSn snapshotNumber []
    atomically $ writeTChan chan msg
