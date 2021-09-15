{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Generator where

import Hydra.Prelude hiding (size)

import Control.Monad (foldM)
import Hydra.Ledger (Utxo)
import Hydra.Ledger.Cardano (
  CardanoKeyPair,
  CardanoTx,
  genFixedSizeSequenceOfValidTransactions,
  genKeyPair,
  genOneUtxoFor,
  genUtxo,
  generateWith,
  mkSimpleCardanoTx,
  mkVkAddress,
  utxoFromTx,
  utxoToList,
  utxoValue,
  verificationKey,
 )
import Test.QuickCheck (Gen, elements, generate)

-- | A 'Dataset' that can be run for testing purpose.
-- The 'transactionSequence' is guaranteed to be applicable, in sequence, to the 'initialUtxo'
-- set.
data Dataset = Dataset
  { initialUtxo :: Utxo CardanoTx
  , transactionsSequence :: [CardanoTx]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Generate an arbitrary UTXO set and a sequence of transactions for this set.
generateDataset :: Int -> IO Dataset
generateDataset sequenceLength = do
  initialUtxo <- generate genUtxo
  transactionsSequence <- generate $ genFixedSizeSequenceOfValidTransactions sequenceLength initialUtxo
  pure Dataset{initialUtxo, transactionsSequence}

-- | Generate a 'Dataset' which does not grow the UTXO set over time.
generateConstantUtxoDataset :: Int -> IO Dataset
generateConstantUtxoDataset = generate . genConstantUtxoDataset

genConstantUtxoDataset :: Int -> Gen Dataset
genConstantUtxoDataset len = do
  keyPair <- genKeyPair
  initialUtxo <- genOneUtxoFor (verificationKey keyPair)
  transactionsSequence <- reverse . thrd <$> foldM generateOneTransfer (initialUtxo, keyPair, []) [1 .. len]
  pure $ Dataset{initialUtxo, transactionsSequence}
 where
  thrd (_, _, c) = c
  generateOneTransfer (utxo, keyPair, txs) _ = do
    recipient <- genKeyPair
    -- NOTE(AB): elements is partial, it crashes if given an empty list, We don't expect
    -- this function to be ever used in production, and crash will be caught in tests
    txin <- elements $ utxoToList utxo
    let tx = mkSimpleCardanoTx txin (mkVkAddress (verificationKey recipient), utxoValue utxo) keyPair
        utxo' = utxoFromTx tx
    pure (utxo', recipient, tx : txs)

mkCredentials :: Int -> CardanoKeyPair
mkCredentials = generateWith genKeyPair
