module Hydra.Ledger.SimpleSpec where

import Hydra.Prelude

import Data.List (maximum)
import qualified Data.Set as Set
import Hydra.Ledger (UTxO, applyTransactions)
import Hydra.Ledger.Simple
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, forAllShrink, shrinkList, sublistOf)
import Test.QuickCheck.Gen (Gen, choose)

spec :: Spec
spec = describe "Simple Ledger" $ do
  prop "validates only correctly built transactions" prop_validateCorrectTransactions

prop_validateCorrectTransactions :: Property
prop_validateCorrectTransactions =
  forAllShrink (genSequenceOfValidTransactions mempty) shrinkSequence $ \txs ->
    isRight (applyTransactions simpleLedger mempty txs)

shrinkSequence :: [SimpleTx] -> [[SimpleTx]]
shrinkSequence = shrinkList (const [])
