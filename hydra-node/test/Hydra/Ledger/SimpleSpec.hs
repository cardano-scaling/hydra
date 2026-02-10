module Hydra.Ledger.SimpleSpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "QuickCheck" Test.QuickCheck (Property, choose, forAllShrink, getSize, shrinkList, sublistOf)
import "base" Data.Foldable (maximum)
import "containers" Data.Set qualified as Set
import "hydra-tx" Hydra.Chain.ChainState (ChainSlot (ChainSlot))
import "hydra-tx" Hydra.Tx.IsTx (IsTx (..))

import Hydra.Ledger (applyTransactions)
import Hydra.Ledger.Simple

spec :: Spec
spec =
  prop "validates only correctly built transactions" prop_validateCorrectTransactions

prop_validateCorrectTransactions :: Property
prop_validateCorrectTransactions =
  forAllShrink (genSequenceOfValidTransactions mempty) shrinkSequence $ \txs ->
    isRight (applyTransactions simpleLedger (ChainSlot 0) mempty txs)

shrinkSequence :: [SimpleTx] -> [[SimpleTx]]
shrinkSequence = shrinkList (const [])

genSequenceOfValidTransactions :: UTxOType SimpleTx -> Gen [SimpleTx]
genSequenceOfValidTransactions initialUTxO = do
  n <- fromIntegral <$> getSize
  let maxId = if Set.null initialUTxO then 0 else unSimpleTxOut (maximum initialUTxO)
  numTxs <- choose (1, n)
  foldlM newTx (maxId, initialUTxO, mempty) [1 .. numTxs] >>= \(_, _, txs) -> pure (reverse txs)
 where
  newTx ::
    (TxIdType SimpleTx, UTxOType SimpleTx, [SimpleTx]) ->
    TxIdType SimpleTx ->
    Gen (TxIdType SimpleTx, UTxOType SimpleTx, [SimpleTx])
  newTx (maxId, utxo, txs) txid = do
    (newMax, ins, outs) <- genInputsAndOutputs maxId utxo
    pure (newMax, (utxo Set.\\ ins) `Set.union` outs, SimpleTx txid ins outs : txs)

  genInputsAndOutputs :: Integer -> Set SimpleTxOut -> Gen (Integer, Set SimpleTxOut, Set SimpleTxOut)
  genInputsAndOutputs maxId utxo = do
    ins <- sublistOf (Set.toList utxo)
    numOuts <- choose (1, 10)
    let outs = fmap (+ maxId) [1 .. numOuts]
    pure (maximum outs, Set.fromList ins, Set.fromList $ fmap SimpleTxOut outs)
