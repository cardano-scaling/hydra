module Main where

import Hydra.Network (Host)
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
main = do
  opts <- execParser netOptions
  print opts
