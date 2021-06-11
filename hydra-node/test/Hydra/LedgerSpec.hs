{-# LANGUAGE TypeApplications #-}

-- | Tests for our 'Hydra.Ledger' integration. We do only want to do "smoke" and
-- "sanity" tests here and leave the detailed unit tests to the ledger
-- implementation itself.
module Hydra.LedgerSpec where

import Cardano.Prelude

import Hydra.Ledger (ValidationError (..))
import Hydra.Ledger.MaryTest (
  applyTx,
  mkLedgerEnv,
  testUTxO,
  txInvalid,
  txSimpleTransfer,
 )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "Hydra Ledger (Mary)" $ do
  it "should reject invalid transactions" $ do
    applyTx mkLedgerEnv testUTxO [txInvalid] `shouldBe` Left ValidationError

  it "should validate transactions which simply transfer value" $ do
    applyTx mkLedgerEnv testUTxO [txSimpleTransfer] `shouldSatisfy` isRight
