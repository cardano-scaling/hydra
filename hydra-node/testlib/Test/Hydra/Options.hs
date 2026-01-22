{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Options where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.ByteString qualified as BS
import Data.IP (IP (IPv4), toIPv4w)
import Hydra.Cardano.Api (
  ChainPoint (..),
  SlotNo (..),
  deserialiseFromRawBytes,
  proxyToAsType,
 )

import Hydra.Logging (Verbosity (..))
import Hydra.Options (CardanoChainConfig (..), ChainBackendOptions (..), ChainConfig (..), LedgerConfig (..), OfflineChainConfig (..), RunOptions (..), defaultBlockfrostOptions, defaultDirectOptions)
import Test.Hydra.Logging ()
import Test.Hydra.Network ()
import Test.Hydra.Node.ApiTransactionTimeout ()
import Test.Hydra.Node.DepositPeriod ()
import Test.Hydra.Node.UnsyncedPeriod ()
import Test.Hydra.Tx.Gen ()
import Test.QuickCheck (Positive (..), choose, elements, listOf, listOf1, oneof, vectorOf)
import Test.QuickCheck.Instances ()

instance Arbitrary IP where
  arbitrary = IPv4 . toIPv4w <$> arbitrary
  shrink = genericShrink

instance Arbitrary RunOptions where
  arbitrary = do
    verbosity <- elements [Quiet, Verbose "HydraNode"]
    nodeId <- arbitrary
    listen <- arbitrary
    advertise <- arbitrary
    peers <- reasonablySized arbitrary
    apiHost <- arbitrary
    apiPort <- arbitrary
    tlsCertPath <- oneof [pure Nothing, Just <$> genFilePath "pem"]
    tlsKeyPath <- oneof [pure Nothing, Just <$> genFilePath "key"]
    monitoringPort <- arbitrary
    hydraSigningKey <- genFilePath "sk"
    hydraVerificationKeys <- reasonablySized (listOf (genFilePath "vk"))
    persistenceDir <- genDirPath
    persistenceRotateAfter <- oneof [pure Nothing, Just . Positive . fromInteger <$> choose (1, 100000)]
    chainConfig <- arbitrary
    ledgerConfig <- arbitrary
    whichEtcd <- arbitrary
    apiTransactionTimeout <- arbitrary
    pure $
      RunOptions
        { verbosity
        , nodeId
        , listen
        , advertise
        , peers
        , apiHost
        , apiPort
        , tlsCertPath
        , tlsKeyPath
        , monitoringPort
        , hydraSigningKey
        , hydraVerificationKeys
        , persistenceDir
        , persistenceRotateAfter
        , chainConfig
        , ledgerConfig
        , whichEtcd
        , apiTransactionTimeout
        }

  shrink = genericShrink

instance Arbitrary LedgerConfig where
  arbitrary = do
    cardanoLedgerProtocolParametersFile <- genFilePath "json"
    pure $ CardanoLedgerConfig{cardanoLedgerProtocolParametersFile}

instance Arbitrary ChainConfig where
  arbitrary =
    oneof
      [ Cardano <$> genCardanoChainConfig
      , Offline <$> genOfflineChainConfig
      ]
   where
    genCardanoChainConfig = do
      hydraScriptsTxId <- reasonablySized arbitrary
      cardanoSigningKey <- genFilePath "sk"
      cardanoVerificationKeys <- reasonablySized (listOf (genFilePath "vk"))
      startChainFrom <- oneof [pure Nothing, Just <$> genChainPoint]
      contestationPeriod <- arbitrary
      depositPeriod <- arbitrary
      unsyncedPeriod <- arbitrary
      chainBackendOptions <-
        oneof
          [ pure $ Direct defaultDirectOptions
          , pure $ Blockfrost defaultBlockfrostOptions
          ]
      pure
        CardanoChainConfig
          { hydraScriptsTxId
          , cardanoSigningKey
          , cardanoVerificationKeys
          , startChainFrom
          , contestationPeriod
          , depositPeriod
          , unsyncedPeriod
          , chainBackendOptions
          }

    genOfflineChainConfig = do
      offlineHeadSeed <- arbitrary
      ledgerGenesisFile <- oneof [pure Nothing, Just <$> genFilePath "json"]
      initialUTxOFile <- genFilePath "json"
      pure
        OfflineChainConfig
          { offlineHeadSeed
          , initialUTxOFile
          , ledgerGenesisFile
          }

genFilePath :: String -> Gen FilePath
genFilePath extension = do
  path <- reasonablySized (listOf1 (elements ["a", "b", "c"]))
  pure $ intercalate "/" path <> "." <> extension

genDirPath :: Gen FilePath
genDirPath = do
  path <- reasonablySized (listOf1 (elements ["a", "b", "c"]))
  pure $ intercalate "/" path

genChainPoint :: Gen ChainPoint
genChainPoint = do
  slotNo <- arbitrary
  genChainPointAt slotNo

genChainPointAt :: SlotNo -> Gen ChainPoint
genChainPointAt slotNo =
  ChainPoint slotNo <$> someHeaderHash
 where
  someHeaderHash = do
    bytes <- vectorOf 32 arbitrary
    let hash = either (error "invalid bytes") id $ deserialiseFromRawBytes (proxyToAsType Proxy) . BS.pack $ bytes
    pure hash
