{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Options (
module Hydra.Options.Common,
  module Hydra.Options.Offline,
  module Hydra.Options.Online,
  module Hydra.Options,
  ParserResult (..),
  renderFailure,
) where

import Hydra.Prelude

import Data.Version (Version (..), showVersion)
import Hydra.Cardano.Api (
  NetworkId (..),
  SocketPath,
 )
import Hydra.Contract qualified as Contract
import Hydra.Ledger.Cardano ()
import Hydra.Options.Common (
  LedgerConfig(..),
  InvalidOptions(..),
  defaultLedgerConfig,
  ledgerConfigParser,
  hydraVerificationKeyFileParser,
  hydraSigningKeyFileParser,
  cardanoVerificationKeyFileParser,
  verbosityParser,
  hostParser,
  portParser,
  apiHostParser,
  apiPortParser,
  monitoringPortParser,
  persistenceDirParser,
  genChainPoint,
  genFilePath,
  genDirPath
 )
import Hydra.Options.Offline (
  RunOfflineOptions (..),
  OfflineConfig (..),
  validateRunOfflineOptions,
  defaultOfflineConfig,
  offlineOptionsParser,
  runOfflineOptionsParser,
 )
import Hydra.Options.Online (
  ChainConfig(..),
  RunOptions(..),
  validateRunOptions,
  defaultContestationPeriod,
  runOptionsParser,
  networkIdParser,
  nodeSocketParser,
  cardanoSigningKeyFileParser,
  defaultChainConfig,
  startChainFromParser,
  toArgNetworkId
 )
import Hydra.Options.Online qualified as OnlineOptions
import Hydra.Version (embeddedRevision, gitRevision, unknownVersion)
import Options.Applicative (
  Parser,
  ParserInfo,
  ParserResult (..),
  command,
  defaultPrefs,
  execParserPure,
  footer,
  fullDesc,
  handleParseResult,
  header,
  help,
  helper,
  hsubparser,
  info,
  infoOption,
  long,
  metavar,
  progDesc,
  progDescDoc,
  renderFailure,
  strOption,
  subparser,
  value,
 )
import Options.Applicative.Help (vsep)
import Paths_hydra_node (version)

data Command
  = Run OnlineOptions.RunOptions
  | RunOffline RunOfflineOptions
  | Publish PublishOptions
  | GenHydraKey GenerateKeyPair
  deriving stock (Show, Eq)

commandParser :: Parser Command
commandParser =
  asum
    [ Run <$> runOptionsParser
    , RunOffline <$> runOfflineOptionsParser
    , Publish <$> publishScriptsParser
    , GenHydraKey <$> genHydraKeyParser
    ]
 where
  publishScriptsParser :: Parser PublishOptions
  publishScriptsParser =
    subparser $
      command
        "publish-scripts"
        ( info
            (helper <*> publishOptionsParser)
            ( fullDesc
                <> progDescDoc
                  ( Just $
                      vsep
                        [ "Publish Hydra's Plutus scripts on chain to be used"
                        , "by the hydra-node as --hydra-script-tx-id."
                        , ""
                        , " ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓ "
                        , " ┃              ⚠ WARNING ⚠              ┃ "
                        , " ┣═══════════════════════════════════════┫ "
                        , " ┃    This costs money. About 50 Ada.    ┃ "
                        , " ┃ Spent using the provided signing key. ┃ "
                        , " ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ "
                        ]
                  )
                <> footer
                  "The command outputs the transaction id (in base16) \
                  \of the publishing transaction. This transaction id \
                  \can then be passed onto '--hydra-scripts-tx-id' to \
                  \start a hydra-node using the referenced scripts."
            )
        )

data PublishOptions = PublishOptions
  { publishNetworkId :: NetworkId
  , publishNodeSocket :: SocketPath
  , publishSigningKey :: FilePath
  }
  deriving stock (Show, Eq)

publishOptionsParser :: Parser PublishOptions
publishOptionsParser =
  PublishOptions
    <$> networkIdParser
    <*> nodeSocketParser
    <*> cardanoSigningKeyFileParser

newtype GenerateKeyPair = GenerateKeyPair
  { outputFile :: FilePath
  }
  deriving stock (Eq, Show)

genHydraKeyParser :: Parser GenerateKeyPair
genHydraKeyParser =
  hsubparser
    ( command
        "gen-hydra-key"
        ( info
            (helper <*> (GenerateKeyPair <$> outputFileParser))
            (progDesc "Generate a pair of Hydra signing/verification keys (off-chain keys).")
        )
    )

outputFileParser :: Parser FilePath
outputFileParser =
  strOption
    ( long "output-file"
        <> metavar "FILE"
        <> value "hydra-key"
        <> help "Basename of files to generate key-pair into. Signing key will be suffixed '.sk' and verification key '.vk'"
    )


hydraNodeCommand :: ParserInfo Command
hydraNodeCommand =
  info
    ( commandParser
        <**> versionInfo
        <**> scriptInfo
        <**> helper
    )
    ( fullDesc
        <> progDesc "Starts a Hydra Node"
        <> header "hydra-node - Implementation of the Hydra Head protocol"
    )
 where
  versionInfo =
    infoOption
      (showVersion hydraNodeVersion)
      (long "version" <> help "Show version")

  scriptInfo =
    infoOption
      (decodeUtf8 $ encodePretty Contract.scriptInfo)
      (long "script-info" <> help "Dump script info as JSON")

hydraNodeVersion :: Version
hydraNodeVersion =
  version & \(Version semver _) -> Version semver revision
 where
  revision =
    maybeToList $
      embeddedRevision
        <|> gitRevision
        <|> Just unknownVersion



-- | Parse command-line arguments into a `Option` or exit with failure and error message.
parseHydraCommand :: IO Command
parseHydraCommand = getArgs <&> parseHydraCommandFromArgs >>= handleParseResult

-- | Pure parsing of `Option` from a list of arguments.
parseHydraCommandFromArgs :: [String] -> ParserResult Command
parseHydraCommandFromArgs = execParserPure defaultPrefs hydraNodeCommand


