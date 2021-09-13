{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Generator where

import Hydra.Prelude hiding (size)

import Control.Monad (foldM)
import qualified Data.List as List
import Hydra.Ledger (Utxo)
import Hydra.Ledger.Cardano (
  CardanoKeyPair,
  CardanoTx,
  genFixedSizeSequenceOfValidTransactions,
  genKeyPair,
  genOneUtxoFor,
  genUtxo,
  mkSimpleCardanoTx,
  mkVkAddress,
  utxoFromTx,
  utxoToList,
  utxoValue,
  verificationKey,
 )
import Test.QuickCheck (Gen, generate)
import Test.QuickCheck.Gen (Gen (MkGen))
import Test.QuickCheck.Random (mkQCGen)

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
    let txin = List.head $ utxoToList utxo
        tx = mkSimpleCardanoTx txin (mkVkAddress (verificationKey recipient), utxoValue utxo) keyPair
        utxo' = utxoFromTx tx
    pure (utxo', recipient, tx : txs)

mkCredentials :: Int -> CardanoKeyPair
mkCredentials = generateWith genKeyPair

generateWith :: Gen a -> Int -> a
generateWith (MkGen runGen) seed =
  runGen (mkQCGen seed) 30
