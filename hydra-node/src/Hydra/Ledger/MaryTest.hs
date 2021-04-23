{-# LANGUAGE TypeApplications #-}

-- | A dummy "Mary" ledger useful for testing purpose
--
--TODO: Move it to dedicated package
module Hydra.Ledger.MaryTest where

import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.ShelleyMA.TxBody (TxBody (TxBody))
import qualified Cardano.Ledger.Val as Val
import Cardano.Prelude
import Data.Default (def)
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.API (Addr, Coin (..), KeyPair, KeyRole (Payment, Staking), StrictMaybe (SNothing), TxId, TxIn (TxIn), TxOut (..), UTxO, Wdrl (..))
import qualified Shelley.Spec.Ledger.API as Ledger
import qualified Shelley.Spec.Ledger.API as Ledgers
import Shelley.Spec.Ledger.Keys (KeyPair (KeyPair))
import Shelley.Spec.Ledger.LedgerState (LedgerState (..), UTxOState (..))
import Shelley.Spec.Ledger.PParams (emptyPParams)
import Shelley.Spec.Ledger.Slot (SlotNo (SlotNo))
import Shelley.Spec.Ledger.UTxO (UTxO (..), txid)
import Test.Cardano.Ledger.EraBuffet (MaryEra, TestCrypto)
import Test.Shelley.Spec.Ledger.Utils (mkAddr, mkKeyPair)

mkLedgerEnv :: Ledgers.LedgersEnv MaryTest
mkLedgerEnv =
  Ledgers.LedgersEnv
    { Ledgers.ledgersSlotNo = SlotNo 1
    , Ledgers.ledgersPp = emptyPParams
    , Ledgers.ledgersAccount = panic "Not implemented"
    }

mkLedgerState :: Ledger.LedgerState MaryTest
mkLedgerState =
  def{_utxoState = def{_utxo = initUTxO}}

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
