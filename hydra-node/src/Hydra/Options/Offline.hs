{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Options.Offline (
  module Hydra.Options.Offline,
) where

import Hydra.Prelude

import Hydra.Logging (Verbosity (..))
import Hydra.Network (PortNumber)
import Hydra.Options.Common (
  InvalidOptions (MaximumNumberOfPartiesExceeded),
  LedgerConfig (..),
  apiHostParser,
  apiPortParser,
  genDirPath,
  genFilePath,
  hostParser,
  hydraSigningKeyFileParser,
  hydraVerificationKeyFileParser,
  ledgerConfigParser,
  monitoringPortParser,
  persistenceDirParser,
  portParser,
  verbosityParser,
 )

import Data.IP (IP (..))
import Options.Applicative (
  Parser,
  command,
  help,
  info,
  long,
  metavar,
  option,
  progDesc,
  showDefault,
  str,
  subparser,
  value,
 )
import Options.Applicative.Extra (helper)
import Test.QuickCheck (listOf)
import Test.QuickCheck.Gen (elements, oneof)

data RunOfflineOptions = RunOfflineOptions
  { verbosity :: Verbosity
  , host :: IP
  , port :: PortNumber
  , apiHost :: IP
  , apiPort :: PortNumber
  , monitoringPort :: Maybe PortNumber
  , hydraSigningKey :: FilePath
  , hydraVerificationKeys :: [FilePath] -- under normal configuration this should be a singleton
  , persistenceDir :: FilePath
  , ledgerConfig :: LedgerConfig
  , offlineConfig :: OfflineConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Convert an 'Options' instance into the corresponding list of command-line arguments.
--
-- This is useful in situations where one wants to programatically define 'Options', providing
-- some measure of type safety, without having to juggle with strings.
toArgs :: RunOfflineOptions -> [String]
toArgs
  RunOfflineOptions
    { verbosity
    , host
    , port
    , apiHost
    , apiPort
    , monitoringPort
    , hydraSigningKey
    , hydraVerificationKeys
    , persistenceDir
    , ledgerConfig
    , offlineConfig
    } =
    ["offline"]
      <> isVerbose verbosity
      <> ["--host", show host]
      <> ["--port", show port]
      <> ["--api-host", show apiHost]
      <> ["--api-port", show apiPort]
      <> ["--hydra-signing-key", hydraSigningKey]
      <> concatMap (\vk -> ["--hydra-verification-key", vk]) hydraVerificationKeys
      <> maybe [] (\mport -> ["--monitoring-port", show mport]) monitoringPort
      <> ["--persistence-dir", persistenceDir]
      <> argsLedgerConfig
      <> argsOfflineConfig
   where
    isVerbose = \case
      Quiet -> ["--quiet"]
      _ -> []

    argsLedgerConfig =
      ["--ledger-protocol-parameters", cardanoLedgerProtocolParametersFile]

    CardanoLedgerConfig
      { cardanoLedgerProtocolParametersFile
      } = ledgerConfig

    argsOfflineConfig =
      ["--initial-utxo", initialUTxOFile offlineConfig]
        <> maybe [] (\s -> ["--ledger-genesis", s]) (ledgerGenesisFile offlineConfig)

instance Arbitrary OfflineConfig where
  arbitrary = do
    ledgerGenesisFile <- oneof [pure Nothing, Just <$> genFilePath "ledgerGenesis"]
    initialUTxOFile <- genFilePath "utxo.json"

    pure $
      OfflineConfig
        { initialUTxOFile
        , ledgerGenesisFile
        }

  shrink = genericShrink

instance Arbitrary RunOfflineOptions where
  arbitrary = do
    verbosity <- elements [Quiet, Verbose "HydraNode"]
    host <- arbitrary
    port <- arbitrary
    apiHost <- arbitrary
    apiPort <- arbitrary
    monitoringPort <- arbitrary
    hydraSigningKey <- genFilePath "sk"
    hydraVerificationKeys <- reasonablySized (listOf (genFilePath "vk"))
    persistenceDir <- genDirPath
    ledgerConfig <- arbitrary
    offlineConfig <- arbitrary
    pure RunOfflineOptions{..}
  shrink = genericShrink

validateRunOfflineOptions :: RunOfflineOptions -> Either InvalidOptions ()
validateRunOfflineOptions RunOfflineOptions{hydraVerificationKeys}
  | numberOfOtherParties > 0 = Left MaximumNumberOfPartiesExceeded
  | otherwise = Right ()
 where
  numberOfOtherParties = length hydraVerificationKeys

runOfflineOptionsParser :: Parser RunOfflineOptions
runOfflineOptionsParser =
  subparser $
    command "offline" $
      info
        ( helper
            <*> ( RunOfflineOptions
                    <$> verbosityParser
                    <*> hostParser
                    <*> portParser
                    <*> apiHostParser
                    <*> apiPortParser
                    <*> optional monitoringPortParser
                    <*> hydraSigningKeyFileParser
                    <*> many hydraVerificationKeyFileParser
                    <*> persistenceDirParser
                    <*> ledgerConfigParser
                    <*> offlineOptionsParser
                )
        )
        (progDesc "Run Hydra Head in offline mode.")

data OfflineConfig = OfflineConfig
  { initialUTxOFile :: FilePath
  , ledgerGenesisFile :: Maybe FilePath
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

defaultOfflineConfig :: OfflineConfig
defaultOfflineConfig =
  OfflineConfig
    { initialUTxOFile = "utxo.json"
    , ledgerGenesisFile = Nothing
    }

offlineOptionsParser :: Parser OfflineConfig
offlineOptionsParser =
  OfflineConfig
    <$> initialUTxOFileParser
    <*> ledgerGenesisFileParser

initialUTxOFileParser :: Parser FilePath
initialUTxOFileParser =
  option
    str
    ( long "initial-utxo"
        <> metavar "FILE"
        <> value "utxo.json"
        <> showDefault
        <> help "File containing initial UTxO for the L2 chain."
    )

ledgerGenesisFileParser :: Parser (Maybe FilePath)
ledgerGenesisFileParser =
  option
    (optional str)
    ( long "ledger-genesis"
        <> metavar "FILE"
        <> value Nothing
        <> showDefault
        <> help "File containing ledger genesis parameters."
    )
