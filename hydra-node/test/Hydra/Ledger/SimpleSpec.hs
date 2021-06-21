module Hydra.Ledger.SimpleSpec where

import Hydra.Prelude

import Data.List (maximum)
import qualified Data.Set as Set
import Hydra.Ledger (UTxO, applyTransactions)
import Hydra.Ledger.Simple
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, arbitrary, forAllShrink, getNonEmpty, shrinkList, sublistOf)
import Test.QuickCheck.Gen (Gen, choose)

spec :: Spec
spec = describe "Simple Ledger" $ do
  prop "validates only correctly built transactions" prop_validateCorrectTransactions

prop_validateCorrectTransactions :: Property
prop_validateCorrectTransactions =
  forAllShrink (sequenceOfValidTransactions mempty) shrinkSequence $ \txs ->
    isRight (applyTransactions simpleLedger mempty txs)

shrinkSequence :: [SimpleTx] -> [[SimpleTx]]
shrinkSequence = shrinkList (const [])

listOfCommittedUtxos :: Integer -> Gen [UTxO SimpleTx]
listOfCommittedUtxos numCommits =
  pure $ Set.singleton . TxIn <$> [1 .. numCommits]

sequenceOfValidTransactions :: UTxO SimpleTx -> Gen [SimpleTx]
sequenceOfValidTransactions initialUtxo = do
  let maxId = if Set.null initialUtxo then 0 else unTxIn (maximum initialUtxo)
  numTxs <- choose (1, 10)
  foldlM newTx (maxId, initialUtxo, mempty) [1 .. numTxs] >>= \(_, _, txs) -> pure (reverse txs)

newTx :: (Integer, UTxO SimpleTx, [SimpleTx]) -> Integer -> Gen (Integer, UTxO SimpleTx, [SimpleTx])
newTx (maxId, utxo, txs) txid = do
  (newMax, ins, outs) <- genInputsAndOutputs maxId utxo
  pure (newMax, (utxo Set.\\ ins) `Set.union` outs, SimpleTx txid ins outs : txs)

genUtxo :: Gen (UTxO SimpleTx)
genUtxo = Set.fromList . fmap TxIn . getNonEmpty <$> arbitrary

genSimpleTx :: Gen SimpleTx
genSimpleTx = do
  utxo <- genUtxo
  (_, ins, outs) <- genInputsAndOutputs (unTxIn $ maximum utxo) utxo
  txid <- arbitrary
  pure $ SimpleTx txid ins outs

genInputsAndOutputs :: Integer -> Set TxIn -> Gen (Integer, Set TxIn, Set TxIn)
genInputsAndOutputs maxId utxo = do
  ins <- sublistOf (Set.toList utxo)
  numOuts <- choose (1, 10)
  let outs = fmap (+ maxId) [1 .. numOuts]
  pure (maximum outs, Set.fromList ins, Set.fromList $ fmap TxIn outs)
