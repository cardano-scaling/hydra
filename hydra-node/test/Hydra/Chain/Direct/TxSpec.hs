-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api (
  makeTransactionBody,
 )
import Gen.Cardano.Api.Typed (genTxIn)
import Hydra.Chain.Direct.Tx (initTx)
import Test.QuickCheck (counterexample, forAll, property)
import Test.QuickCheck.Hedgehog (hedgehog)

spec :: Spec
spec =
  parallel $
    describe "initTx" $ do
      prop "can be used to construct a non-balanced TxBody" $ \params ->
        forAll (hedgehog genTxIn) $ \txIn ->
          case makeTransactionBody $ initTx params txIn of
            Left err -> counterexample ("TxBodyError: " <> show err) False
            Right _ -> property True
