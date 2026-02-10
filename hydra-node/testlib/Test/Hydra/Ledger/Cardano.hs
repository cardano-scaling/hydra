{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Ledger.Cardano where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude

import "QuickCheck" Test.QuickCheck (choose, getSize, vectorOf)
import "base" Control.Monad (foldM)
import "bytestring" Data.ByteString qualified as BS
import "hydra-cardano-api" Hydra.Cardano.Api hiding (initialLedgerState, utxoFromTx)
import "hydra-tx" Hydra.Ledger.Cardano (mkTransferTx)
import "hydra-tx" Hydra.Tx (IsTx (..))
import "hydra-tx" Test.Hydra.Tx.Fixture (testNetworkId)
import "hydra-tx" Test.Hydra.Tx.Gen (genKeyPair, genOneUTxOFor)
import "z-cardano-ledger-babbage-z-testlib" Test.Cardano.Ledger.Babbage.Arbitrary ()
import "z-cardano-ledger-conway-z-testlib" Test.Cardano.Ledger.Conway.Arbitrary ()

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
