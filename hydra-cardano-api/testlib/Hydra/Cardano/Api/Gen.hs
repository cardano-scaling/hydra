{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.Gen (
  genTxIn,
) where

import "QuickCheck" Test.QuickCheck (Arbitrary (..), Gen, choose, oneof, vectorOf)
import "bytestring" Data.ByteString qualified as BS
import "cardano-api" Cardano.Api
import "cardano-ledger-binary" Cardano.Ledger.Binary qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.BaseTypes qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.TxIn qualified as Ledger
import "hedgehog-quickcheck" Test.QuickCheck.Hedgehog (hedgehog)
import "hydra-cardano-api" Hydra.Cardano.Api.TxIn (fromLedgerTxIn)
import "z-cardano-api-z-gen" Test.Gen.Cardano.Api.Typed qualified as Gen

-- * Orphans

instance Arbitrary (Address ByronAddr) where
  arbitrary = hedgehog Gen.genAddressByron

instance Arbitrary ChainPoint where
  arbitrary = hedgehog Gen.genChainPoint

instance Arbitrary NetworkMagic where
  arbitrary = NetworkMagic <$> arbitrary
  shrink (NetworkMagic x) = NetworkMagic <$> shrink x

instance Arbitrary NetworkId where
  arbitrary = oneof [pure Mainnet, Testnet <$> arbitrary]
  shrink = \case
    Mainnet -> []
    Testnet magic -> Testnet <$> shrink magic

instance Arbitrary PolicyId where
  arbitrary = hedgehog Gen.genPolicyId

-- | A more random generator than the 'Arbitrary TxIn' from cardano-ledger.
-- NOTE: This is using the Cardano ledger's deserialization framework using the
-- latest protocol version via 'maxBound'.
genTxIn :: Gen TxIn
genTxIn =
  fmap fromLedgerTxIn . Ledger.TxIn
    -- NOTE: [88, 32] is a CBOR prefix for a bytestring of 32 bytes.
    <$> fmap (Ledger.unsafeDeserialize' maxBound . BS.pack . ([88, 32] <>)) (vectorOf 32 arbitrary)
    <*> fmap Ledger.TxIx (choose (0, 99))

instance Arbitrary TxIn where
  arbitrary = genTxIn
