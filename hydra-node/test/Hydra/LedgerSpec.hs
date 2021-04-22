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
import Cardano.Ledger.Val (Val ((<+>)), (<->))
import qualified Cardano.Ledger.Val as Val
import Data.Default (def)
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.API (Addr, Coin (..), KeyPair (..), KeyRole (Payment, Staking), StrictMaybe (SNothing), Tx (..), TxId, TxIn (TxIn), TxOut (..), UTxO, Wdrl (..))
import qualified Shelley.Spec.Ledger.API as Ledger
import Shelley.Spec.Ledger.Keys (asWitness)
import Shelley.Spec.Ledger.LedgerState (LedgerState (..), UTxOState (..))
import Shelley.Spec.Ledger.Tx (addrWits)
import Shelley.Spec.Ledger.UTxO (UTxO (..), makeWitnessesVKey, txid)
import Test.Cardano.Ledger.EraBuffet (MaryEra, TestCrypto)
import Test.Shelley.Spec.Ledger.Utils (mkAddr, mkKeyPair)

spec :: Spec
spec = describe "Hydra Ledger (Mary)" $
  it "should validate transactions which simply transfer value" $ do
    validateTx mkLedgerState txSimpleTransfer `shouldBe` Valid

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
      [ TxOut aliceAddr (Val.inject aliceInitCoin <-> Val.inject feeEx <-> val) -- TODO(SN): remove fee?
      , TxOut bobAddr (Val.inject bobInitCoin <+> val)
      ]
      unboundedInterval
      Val.zero

  val = Val.inject (Coin 5)

mkLedgerState :: Ledger.LedgerState MaryTest
mkLedgerState =
  def{_utxoState = def{_utxo = initUTxO}}

--
-- From: shelley-ma/shelley-ma-test/test/Test/Cardano/Ledger/Mary/Examples/Cast.hs
--

-- | Alice's payment key pair
alicePay :: KeyPair 'Payment TestCrypto
alicePay = KeyPair vk sk
 where
  (sk, vk) = mkKeyPair (0, 0, 0, 0, 0)

-- | Alice's stake key pair
aliceStake :: KeyPair 'Staking TestCrypto
aliceStake = KeyPair vk sk
 where
  (sk, vk) = mkKeyPair (1, 1, 1, 1, 1)

-- | Alice's base address
aliceAddr :: Addr TestCrypto
aliceAddr = mkAddr (alicePay, aliceStake)

-- | Bob's payment key pair
bobPay :: KeyPair 'Payment TestCrypto
bobPay = KeyPair vk sk
 where
  (sk, vk) = mkKeyPair (2, 2, 2, 2, 2)

-- | Bob's stake key pair
bobStake :: KeyPair 'Staking TestCrypto
bobStake = KeyPair vk sk
 where
  (sk, vk) = mkKeyPair (3, 3, 3, 3, 3)

-- | Bob's address
bobAddr :: Addr TestCrypto
bobAddr = mkAddr (bobPay, bobStake)

--
-- From: shelley-ma/shelley-ma-test/test/Test/Cardano/Ledger/Mary/Examples/MultiAssets.hs
--

type MaryTest = MaryEra TestCrypto

aliceInitCoin :: Coin
aliceInitCoin = Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

bobInitCoin :: Coin
bobInitCoin = Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

unboundedInterval :: ValidityInterval
unboundedInterval = ValidityInterval SNothing SNothing

bootstrapTxId :: TxId TestCrypto
bootstrapTxId = txid @MaryTest txb
 where
  txb :: TxBody MaryTest
  txb =
    TxBody
      Set.empty
      StrictSeq.empty
      StrictSeq.empty
      (Wdrl Map.empty)
      (Coin 0)
      unboundedInterval
      SNothing
      SNothing
      (Val.inject (Coin 0))

initUTxO :: UTxO MaryTest
initUTxO =
  UTxO $
    Map.fromList
      [ (TxIn bootstrapTxId 0, TxOut aliceAddr (Val.inject aliceInitCoin))
      , (TxIn bootstrapTxId 1, TxOut bobAddr (Val.inject bobInitCoin))
      ]

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
