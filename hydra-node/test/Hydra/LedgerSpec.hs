{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- | Tests for our 'Hydra.Ledger' integration. We do only want to do "smoke" and
-- "sanity" tests here and leave the detailed unit tests to the ledger
-- implementation itself.
module Hydra.LedgerSpec where

import Cardano.Prelude
import Hydra.Ledger

import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
 )

-- REVIEW(SN): use a more consistent set of ledger imports, but some things not
-- in the API?

import Cardano.Ledger.Core (Value)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.ShelleyMA.TxBody (TxBody (TxBody))
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.Ledger.MaryTest
import Shelley.Spec.Ledger.API (Coin (..), StrictMaybe (SNothing), Tx (..), TxIn (TxIn), TxOut (..), Wdrl (..))
import qualified Shelley.Spec.Ledger.API as Ledger
import Shelley.Spec.Ledger.Keys (asWitness)
import Shelley.Spec.Ledger.Tx (addrWits)
import Shelley.Spec.Ledger.UTxO (makeWitnessesVKey)
import Test.Cardano.Ledger.EraBuffet (TestCrypto)

spec :: Spec
spec = describe "Hydra Ledger (Mary)" $ do
  it "should reject invalid transactions" $ do
    validateTx mkLedgerEnv mkLedgerState txInvalid `shouldBe` Invalid ValidationError

  it "should validate transactions which simply transfer value" $ do
    validateTx mkLedgerEnv mkLedgerState txSimpleTransfer `shouldBe` Valid

-- | Some invalid tx (unbalanced and no witnesses).
txInvalid :: Ledger.Tx MaryTest
txInvalid = Tx (makeTxb [TxIn bootstrapTxId 0] [] unboundedInterval Val.zero) mempty SNothing

-- | Alice gives five Ada to Bob.
-- TODO(SN): type Era = Mary TestCrypt as era instead?
txSimpleTransfer :: Ledger.Tx MaryTest
txSimpleTransfer =
  Tx
    txbody
    mempty{addrWits = makeWitnessesVKey (hashAnnotated txbody) [asWitness alicePay]}
    SNothing
 where
  txbody :: TxBody MaryTest
  txbody =
    makeTxb
      [TxIn bootstrapTxId 0]
      [ TxOut aliceAddr $ Val.inject $ aliceInitCoin <-> feeEx <-> transfered -- TODO(SN): remove fee?
      , TxOut bobAddr $ Val.inject transfered
      ]
      unboundedInterval
      Val.zero

  transfered = Coin 5

--
-- From: shelley-ma/shelley-ma-test/test/Test/Cardano/Ledger/Mary/Examples/Cast.hs
--
feeEx :: Coin
feeEx = Coin 3

-- These examples do not use several of the transaction components,
-- so we can simplify building them.
makeTxb ::
  [TxIn TestCrypto] ->
  [TxOut MaryTest] ->
  ValidityInterval ->
  Value MaryTest ->
  TxBody MaryTest
makeTxb ins outs interval =
  TxBody
    (Set.fromList ins)
    (StrictSeq.fromList outs)
    StrictSeq.empty
    (Wdrl Map.empty)
    feeEx
    interval
    SNothing
    SNothing
