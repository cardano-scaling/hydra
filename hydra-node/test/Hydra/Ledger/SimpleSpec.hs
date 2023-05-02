module Hydra.Ledger.SimpleSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Ledger (ChainSlot (..), applyTransactions)
import Hydra.Ledger.Simple
import Test.QuickCheck (Property, forAllShrink, shrinkList)

spec :: Spec
spec =
  prop "validates only correctly built transactions" prop_validateCorrectTransactions

prop_validateCorrectTransactions :: Property
prop_validateCorrectTransactions =
  forAllShrink (genSequenceOfValidTransactions mempty) shrinkSequence $ \txs ->
    isRight (applyTransactions simpleLedger (ChainSlot 0) mempty txs)

shrinkSequence :: [SimpleTx] -> [[SimpleTx]]
shrinkSequence = shrinkList (const [])
