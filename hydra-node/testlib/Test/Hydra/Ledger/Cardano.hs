{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Ledger.Cardano where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api hiding (initialLedgerState, utxoFromTx)
import Hydra.Ledger.Cardano (mkTransferTx)
import Hydra.Tx (IsTx (..))
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.Hydra.Tx.Gen (genKeyPair, genOneUTxOFor)
import Test.QuickCheck (choose, getSize, vectorOf)
import "base" Control.Monad (foldM)
import "bytestring" Data.ByteString qualified as BS

-- * Generators

-- | Generates a sequence of simple "transfer" transactions for a single key.
genSequenceOfSimplePaymentTransactions :: Gen (UTxO, [Tx])
genSequenceOfSimplePaymentTransactions = do
  n <- getSize
  numTxs <- choose (1, n)
  genFixedSizeSequenceOfSimplePaymentTransactions numTxs

genFixedSizeSequenceOfSimplePaymentTransactions :: Int -> Gen (UTxO, [Tx])
genFixedSizeSequenceOfSimplePaymentTransactions numTxs = do
  (vk, sk) <- genKeyPair
  utxo <- genOneUTxOFor vk
  (_, txs) <- foldM (go sk) (utxo, []) [1 .. numTxs]
  pure (utxo, reverse txs)

go :: SigningKey PaymentKey -> (UTxO, [Tx]) -> Int -> Gen (UTxO, [Tx])
go sk (utxo, txs) _ = do
  case mkTransferTx testNetworkId utxo sk (getVerificationKey sk) of
    Left err -> error $ "mkTransferTx failed: " <> err
    Right tx -> pure (utxoFromTx tx, tx : txs)

-- * Orphans

instance Arbitrary (Hash PaymentKey) where
  arbitrary = unsafePaymentKeyHashFromBytes . BS.pack <$> vectorOf 28 arbitrary
