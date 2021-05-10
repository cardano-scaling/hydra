{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- | Tests for our 'Hydra.Ledger' integration. We do only want to do "smoke" and
-- "sanity" tests here and leave the detailed unit tests to the ledger
-- implementation itself.
module Hydra.LedgerSpec where

import Cardano.Prelude

import Hydra.Ledger (ValidationError (..), ValidationResult (..), validateTx)
import Hydra.Ledger.MaryTest (
  mkLedgerEnv,
  mkLedgerState,
  txInvalid,
  txSimpleTransfer,
 )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Hydra Ledger (Mary)" $ do
  it "should reject invalid transactions" $ do
    validateTx mkLedgerEnv mkLedgerState txInvalid `shouldBe` Invalid ValidationError

  it "should validate transactions which simply transfer value" $ do
    validateTx mkLedgerEnv mkLedgerState txSimpleTransfer `shouldBe` Valid
