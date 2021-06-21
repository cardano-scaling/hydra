{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A simplified "Mary" ledger, which is initialized with a genesis UTXO set
-- and also some example transactions. Use this with 'cardanoLedger'.
module Hydra.Ledger.MaryTest where

import Hydra.Prelude

-- REVIEW(SN): use a more consistent set of ledger imports, but some things not
-- in the API?

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
  KeyPair,
  KeyRole (Payment, Staking),
  Network (Testnet),
  StrictMaybe (SNothing),
  TxId,
  TxIn (TxIn),
  TxOut (..),
  Wdrl (..),
 )
import qualified Shelley.Spec.Ledger.API as Ledger
import Cardano.Ledger.BaseTypes (UnitInterval, mkActiveSlotCoeff, mkUnitInterval)
import Cardano.Ledger.Keys (KeyPair (KeyPair), asWitness)
import Shelley.Spec.Ledger.LedgerState (EpochState (..), LedgerState (..), NewEpochState (..), UTxOState (..))
import Shelley.Spec.Ledger.Tx (addrWits)
import Shelley.Spec.Ledger.UTxO (UTxO (..), makeWitnessesVKey, txid)
import Test.Cardano.Ledger.EraBuffet (MaryEra, TestCrypto, Value)
import Test.Shelley.Spec.Ledger.Utils (mkAddr, mkKeyPair)
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

instance Read MaryTestTx where
  readPrec = error "Read: MaryTestTx"

instance Read (Ledger.UTxO era) where
  readPrec = error "Read: Ledger.UTxO"

cardanoLedger :: Ledger (Ledger.Tx MaryTest)
cardanoLedger =
  Ledger
    { applyTransactions = applyTx
    , initUTxO = getUTxO def
    }

applyTx ::
  ( Default (Ledger.UTxOState era)
  , Default (Ledger.LedgerState era)
  , ApplyTx era
  ) =>
  Ledger.UTxO era ->
  [Ledger.Tx era] ->
  Either ValidationError (Ledger.UTxO era)
applyTx utxo txs =
  case Ledger.applyTxs globals (SlotNo 1) (Seq.fromList txs) newEpochState of
    Left err -> Left $ toValidationError err
    Right ls -> Right $ getUTxO $ esLState $ nesEs ls
 where
  -- toValidationError :: ApplyTxError -> ValidationError
  toValidationError = const ValidationError

  -- NOTE(SN): This does rely on that *only* nesEs/esLState is used
  newEpochState =
    NewEpochState
      { nesEs =
          EpochState
            { esAccountState = panic "undefined EpochState"
            , esSnapshots = panic "undefined EpochState"
            , esLState = fromUTxO utxo
            , esPrevPp = panic "undefined EpochState"
            , esPp = panic "undefined EpochState"
            , esNonMyopic = panic "undefined EpochState"
            }
      , nesEL = panic "undefined newEpochState"
      , nesBprev = panic "undefined newEpochState"
      , nesBcur = panic "undefined newEpochState"
      , nesRu = panic "undefined newEpochState"
      , nesPd = panic "undefined newEpochState"
      }

getUTxO :: Ledger.LedgerState era -> Ledger.UTxO era
getUTxO = _utxo . _utxoState

fromUTxO ::
  ( Default (Ledger.UTxOState era)
  , Default (Ledger.LedgerState era)
  ) =>
  Ledger.UTxO era ->
  Ledger.LedgerState era
fromUTxO utxo = def{_utxoState = def{_utxo = utxo}}

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
    , activeSlotCoeff = mkActiveSlotCoeff . unsafeMkUnitInterval $ 0.9
    , networkId = Testnet
    , systemStart = SystemStart $ posixSecondsToUTCTime 0
    }

-- | You vouch that argument is in [0; 1].
unsafeMkUnitInterval :: Ratio Word64 -> UnitInterval
unsafeMkUnitInterval r =
  fromMaybe (error "could not construct unit interval") $ mkUnitInterval r

-- * Test functions
--
-- From: shelley-ma/shelley-ma-test/test/Test/Cardano/Ledger/Mary/Examples/MultiAssets.hs
--

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
