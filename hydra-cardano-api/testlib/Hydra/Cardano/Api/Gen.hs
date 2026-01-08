{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.Gen (
  genTxIn,
) where

import Cardano.Api
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Binary qualified as Ledger
import Cardano.Ledger.TxIn qualified as Ledger
import Data.ByteString qualified as BS
import Hydra.Cardano.Api.TxIn (fromLedgerTxIn)
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.QuickCheck (Arbitrary (..), Gen, choose, oneof, vectorOf)
import Test.QuickCheck.Hedgehog (hedgehog)

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
