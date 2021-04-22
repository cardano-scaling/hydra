module Hydra.LedgerSpec where

import Cardano.Prelude
import Hydra.Ledger

import Test.Hspec (
  Expectation,
  Spec,
  describe,
  it,
  shouldBe,
 )

import qualified Shelley.Spec.Ledger.API as Ledger

spec :: Spec
spec = describe "Hydra Ledger (Mary)" $ do
  it "should validate transactions which simply transfer value" $ do
    validateTx testLedgerState txSimpleTransfer `shouldBe` Valid

txSimpleTransfer :: Ledger.Tx Era
txSimpleTransfer = panic "undefined"

testLedgerState :: Ledger.LedgerState Era
testLedgerstate = panic "undefined"
