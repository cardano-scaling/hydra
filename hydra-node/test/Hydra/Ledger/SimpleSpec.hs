module Hydra.Ledger.SimpleSpec where

import Hydra.Prelude

import Hydra.Ledger (applyTransactions)
import Hydra.Ledger.Simple
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, forAllShrink, shrinkList)

spec :: Spec
spec = describe "Simple Ledger" $ do
  prop "validates only correctly built transactions" prop_validateCorrectTransactions

prop_validateCorrectTransactions :: Property
prop_validateCorrectTransactions =
  forAllShrink (genSequenceOfValidTransactions mempty) shrinkSequence $ \txs ->
    isRight (applyTransactions simpleLedger mempty txs)

shrinkSequence :: [SimpleTx] -> [[SimpleTx]]
shrinkSequence = shrinkList (const [])
