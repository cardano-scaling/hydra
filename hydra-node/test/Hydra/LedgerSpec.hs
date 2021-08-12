{-# LANGUAGE TypeApplications #-}

-- | Tests for our 'Hydra.Ledger' integration. We do only want to do "smoke" and
-- "sanity" tests here and leave the detailed unit tests to the ledger
-- implementation itself.
module Hydra.LedgerSpec where

import Hydra.Prelude

import Hydra.Ledger (ValidationError (..))
import Hydra.Ledger.MaryTest (
  applyTx,
  mkLedgerEnv,
  testUtxo,
  txInvalid,
  txSimpleTransfer,
 )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "Hydra Ledger (Mary)" $ do
  it "should reject invalid transactions" $ do
    applyTx mkLedgerEnv testUtxo [txInvalid] `shouldBe` Left ValidationError

  it "should validate transactions which simply transfer value" $ do
    applyTx mkLedgerEnv testUtxo [txSimpleTransfer] `shouldSatisfy` isRight
