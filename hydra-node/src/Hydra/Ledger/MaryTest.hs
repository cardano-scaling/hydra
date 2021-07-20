{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A simplified "Mary" ledger, which is initialized with a genesis UTXO set
-- and also some example transactions. Use this with 'cardanoLedger'.
module Hydra.Ledger.MaryTest where

import Hydra.Prelude

-- REVIEW(SN): use a more consistent set of ledger imports, but some things not
-- in the API?

import Cardano.Ledger.BaseTypes (mkActiveSlotCoeff)
import Cardano.Ledger.Keys (KeyPair (KeyPair), asWitness)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.ShelleyMA.TxBody (TxBody (TxBody))
import Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo (
  fixedEpochInfo,
 )
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Default (Default, def)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Ledger (Ledger (..), Tx (..), ValidationError (..))
import Shelley.Spec.Ledger.API (
  Addr,
  ApplyTx,
  Coin (..),
  Globals (..),
  KeyRole (Payment, Staking),
  LedgerEnv (..),
  Network (Testnet),
  StrictMaybe (SNothing),
  TxIn (TxIn),
  TxOut (..),
  Wdrl (..),
 )
import qualified Shelley.Spec.Ledger.API as Ledger
import Shelley.Spec.Ledger.LedgerState (UTxOState (..))
import Shelley.Spec.Ledger.Tx (addrWits)
import Shelley.Spec.Ledger.UTxO (UTxO (..), makeWitnessesVKey, txid)
import Test.Cardano.Ledger.EraBuffet (MaryEra, TestCrypto, Value)
import Test.Shelley.Spec.Ledger.Utils (RawSeed(..), mkAddr, mkKeyPair, unsafeBoundRational)
import Text.Read (Read (..))

type MaryTest = MaryEra TestCrypto

type MaryTestTx = Ledger.Tx MaryTest

-- Orphan
instance Semigroup (Ledger.UTxO MaryTest) where
  Ledger.UTxO u1 <> Ledger.UTxO u2 = Ledger.UTxO (u1 <> u2)

instance Monoid (Ledger.UTxO MaryTest) where
  mempty = Ledger.UTxO mempty

instance Tx MaryTestTx where
  type UTxO MaryTestTx = Ledger.UTxO MaryTest
  type TxId MaryTestTx = Ledger.TxId MaryTest

  txId = error "txId : MaryTestTx"

instance Read MaryTestTx where
  readPrec = error "Read: MaryTestTx"

instance Read (Ledger.UTxO era) where
  readPrec = error "Read: Ledger.UTxO"

instance ToJSON MaryTestTx where
  toJSON = error "toJSON: MaryTestTx"

instance FromJSON MaryTestTx where
  parseJSON = error "parseJSON: MaryTestTx"

instance ToJSON (Ledger.UTxO era) where
  toJSON = error "toJSON: Ledger.UTxO"

instance FromJSON (Ledger.UTxO era) where
  parseJSON = error "parseJSON: Ledger.UTxO"

instance Read (Ledger.TxId era) where
  readPrec = error "Read: Ledger.TxId"

instance ToJSON (Ledger.TxId era) where
  toJSON = error "toJSON: Ledger.TxId"

instance FromJSON (Ledger.TxId era) where
  parseJSON = error "parseJSON: Ledger.TxId"

cardanoLedger :: Ledger.LedgerEnv MaryTest -> Ledger (Ledger.Tx MaryTest)
cardanoLedger env =
  Ledger
    { applyTransactions = applyTx env
    , initUTxO = Ledger._utxo def
    }

applyTx ::
  ( Default (Ledger.UTxOState era)
  , ApplyTx era
  ) =>
  Ledger.LedgerEnv era ->
  Ledger.UTxO era ->
  [Ledger.Tx era] ->
  Either ValidationError (Ledger.UTxO era)
applyTx env utxo txs =
  case Ledger.applyTxsTransition globals env (Seq.fromList txs) memPoolState of
    Left err -> Left $ toValidationError err
    Right (ls, _ds) -> Right $ Ledger._utxo ls
 where
  -- toValidationError :: ApplyTxError -> ValidationError
  toValidationError = const ValidationError

  memPoolState = (def{_utxo = utxo}, def)

--
-- From: shelley/chain-and-ledger/shelley-spec-ledger-test/src/Test/Shelley/Spec/Ledger/Utils.hs
--

-- TODO(SN): not hard-code these obviously
globals :: Globals
globals =
  Globals
    { epochInfoWithErr = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)
    , slotsPerKESPeriod = 20
    , stabilityWindow = 33
    , randomnessStabilisationWindow = 33
    , securityParameter = 10
    , maxKESEvo = 10
    , quorum = 5
    , maxMajorPV = 1000
    , maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000
    , activeSlotCoeff = mkActiveSlotCoeff . unsafeBoundRational $ 0.9
    , networkId = Testnet
    , systemStart = SystemStart $ posixSecondsToUTCTime 0
    }

-- * Test functions

mkLedgerEnv :: Ledger.LedgerEnv MaryTest
mkLedgerEnv =
  Ledger.LedgerEnv
    { ledgerSlotNo = SlotNo 1
    , ledgerIx = error "mkLedgerEnv ledgerIx undefinex"
    , ledgerPp = def
    , ledgerAccount = error "mkLedgerenv ledgersAccount undefined"
    }

--
-- From: shelley-ma/shelley-ma-test/test/Test/Cardano/Ledger/Mary/Examples/MultiAssets.hs
--

aliceInitCoin :: Coin
aliceInitCoin = Coin $ 10 * 1000 * 1000 * 1000 * 1000 * 1000

bobInitCoin :: Coin
bobInitCoin = Coin $ 1 * 1000 * 1000 * 1000 * 1000 * 1000

unboundedInterval :: ValidityInterval
unboundedInterval = ValidityInterval SNothing SNothing

bootstrapTxId :: Ledger.TxId TestCrypto
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

testUTxO :: Ledger.UTxO MaryTest
testUTxO =
  UTxO $
    Map.fromList
      [ (TxIn bootstrapTxId 0, TxOut aliceAddr (Val.inject aliceInitCoin))
      , (TxIn bootstrapTxId 1, TxOut bobAddr (Val.inject bobInitCoin))
      ]

-- | Alice's payment key pair
alicePay :: KeyPair 'Payment TestCrypto
alicePay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 0)

-- | Alice's stake key pair
aliceStake :: KeyPair 'Staking TestCrypto
aliceStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 1 1 1 1 1)

-- | Alice's base address
aliceAddr :: Addr TestCrypto
aliceAddr = mkAddr (alicePay, aliceStake)

-- | Bob's payment key pair
bobPay :: KeyPair 'Payment TestCrypto
bobPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 2 2 2 2 2)

-- | Bob's stake key pair
bobStake :: KeyPair 'Staking TestCrypto
bobStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 3 3 3 3 3)

-- | Bob's address
bobAddr :: Addr TestCrypto
bobAddr = mkAddr (bobPay, bobStake)

--
-- From: shelley-ma/shelley-ma-test/test/Test/Cardano/Ledger/Mary/Examples/Cast.hs
--

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

feeEx :: Coin
feeEx = Coin 3

--
-- Example transactions
--

-- | Some invalid tx (unbalanced and no witnesses).
txInvalid :: Ledger.Tx MaryTest
txInvalid = Ledger.Tx (makeTxb [TxIn bootstrapTxId 0] [] unboundedInterval Val.zero) mempty SNothing

-- | Alice gives five Ada to Bob.
txSimpleTransfer :: Ledger.Tx MaryTest
txSimpleTransfer =
  Ledger.Tx
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
