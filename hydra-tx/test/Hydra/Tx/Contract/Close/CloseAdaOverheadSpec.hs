{-# LANGUAGE DuplicateRecordFields #-}

-- | H1 verification: does coverFee_'s ensureMinCoinTxOut bump the head
-- output's lovelace at Close, and if so, does that make the new strict-==
-- mustPreserveHeadValue fail?
--
-- The setup mirrors a real Hydra deployment as closely as possible:
--
--   1. Build the healthy open head TxOut (mkHeadOutput produces only tokens,
--      no lovelace).
--   2. Apply ensureMinCoinTxOut once to simulate what coverFee_ does at
--      Init time. Call the resulting lovelace 'openLovelace'.
--   3. Build the closeTx (which preserves the head value via modifyTxOutDatum
--      only).
--   4. Apply ensureMinCoinTxOut again to the closeTx's head output to simulate
--      what coverFee_ does at Close. Call the resulting lovelace
--      'closeLovelaceAfter'.
--
-- If 'closeLovelaceAfter > openLovelace' then coverFee_ bumps the head
-- value at Close, and the new strict val' == val check in
-- mustPreserveHeadValue (commit 9deddb80a) will fail Close with H4
-- (HeadValueIsNotPreserved) for low-ADA heads.
module Hydra.Tx.Contract.Close.CloseAdaOverheadSpec where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Coin (Coin (..))
import Data.Maybe (fromJust)
import Hydra.Contract.HeadState qualified as Head
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Tx (mkHeadId)
import Hydra.Tx.Close (OpenThreadOutput (..), closeTx)
import Hydra.Tx.Contract.Close.CloseUnused (
  healthyCurrentOpenDatum,
  healthyCurrentSnapshot,
  healthyCurrentSnapshotVersion,
 )
import Hydra.Tx.Contract.Close.Healthy (
  healthyCloseLowerBoundSlot,
  healthyCloseUpperBoundPointInTime,
  healthyConfirmedSnapshot,
  healthyContestationDeadline,
  healthyContestationPeriod,
  healthyOnChainParties,
  healthyOpenHeadTxIn,
  healthyOpenHeadTxOut,
  healthyParticipants,
  somePartyCardanoVerificationKey,
 )
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)
import Hydra.Tx.Utils (IncrementalAction (..), setIncrementalActionMaybe, verificationKeyToOnChainId)
import PlutusLedgerApi.V3 (PubKeyHash (..), toBuiltin)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Gen (ensureSomeAda, genScriptRegistry)

spec :: Spec
spec = describe "Close ADA overhead (H1 verification)" $ do
  it "ensureMinCoinTxOut bumps the head output between Init and Close (or doesn't)" $ do
    let datum = mkTxOutDatumInline healthyCurrentOpenDatum
        rawOpenHead = healthyOpenHeadTxOut datum
        Coin rawLovelace = selectLovelace (txOutValue rawOpenHead)

        -- Simulate post-Init coverFee_: ensureMinCoinTxOut applied to the
        -- mkHeadOutput value, which carries only tokens.
        openHeadOutput = ensureSomeAda rawOpenHead
        Coin openLovelace = selectLovelace (txOutValue openHeadOutput)

        scriptRegistry = genScriptRegistry `generateWith` 42

        snapshot = healthyConfirmedSnapshot healthyCurrentSnapshot

        incAction =
          fromMaybe NoThing $
            setIncrementalActionMaybe
              (utxoToCommit $ getSnapshot snapshot)
              (utxoToDecommit $ getSnapshot snapshot)

        openThread =
          OpenThreadOutput
            { openThreadUTxO = (healthyOpenHeadTxIn, openHeadOutput)
            , openParties = healthyOnChainParties
            , openContestationPeriod = healthyContestationPeriod
            }

        tx =
          closeTx
            scriptRegistry
            somePartyCardanoVerificationKey
            (mkHeadId Fixture.testPolicyId)
            healthyCurrentSnapshotVersion
            snapshot
            healthyCloseLowerBoundSlot
            healthyCloseUpperBoundPointInTime
            openThread
            incAction

        -- closeTx writes the head output at index 0
        closeHeadOutput = fromJust $ txOuts' tx !!? 0
        Coin closeLovelaceBefore = selectLovelace (txOutValue closeHeadOutput)

        -- Simulate post-Close coverFee_: ensureMinCoinTxOut applied to the
        -- closeTx head output (which carries the new ClosedDatum).
        closeHeadOutputAfter = ensureSomeAda (toCtxUTxOTxOut closeHeadOutput)
        Coin closeLovelaceAfter = selectLovelace (txOutValue closeHeadOutputAfter)

        bump = closeLovelaceAfter - openLovelace

    putStrLn $ "raw mkHeadOutput lovelace:                  " <> show rawLovelace
    putStrLn $ "after Init coverFee_ (open head min-coin):  " <> show openLovelace
    putStrLn $ "after closeTx (modifyTxOutDatum only):      " <> show closeLovelaceBefore
    putStrLn $ "after Close coverFee_ (close head min-coin):" <> show closeLovelaceAfter
    putStrLn $ "delta (closeAfter - openAfter):              " <> show bump

    -- closeTx must not change the head value
    closeLovelaceBefore `shouldBe` openLovelace

    -- The H1 question:
    if bump > 0
      then
        expectationFailure $
          "H1 CONFIRMED: ensureMinCoinTxOut would bump the head output at Close by "
            <> show bump
            <> " lovelace. Open head value (post-Init coverFee_): "
            <> show openLovelace
            <> ". Close head value (post-Close coverFee_): "
            <> show closeLovelaceAfter
            <> ". With the new strict val' == val in mustPreserveHeadValue"
            <> " (commit 9deddb80a), this would fail Close with H4"
            <> " (HeadValueIsNotPreserved) for any head whose open value sits"
            <> " exactly at OpenDatum's min-coin and whose ClosedDatum has a"
            <> " larger min-coin."
      else
        -- bump == 0 ⇒ H1 is a phantom: ensureMinCoinTxOut at Close doesn't bump
        --   because OpenDatum's min-coin already exceeds ClosedDatum's min-coin
        --   (or they're equal).
        bump `shouldBe` 0

  it "Contest does not trigger a min-coin bump even when contesters list grows to its maximum" $ do
    -- Mirrors the Close test but advances through a worst-case Contest:
    -- the ClosedDatum's `contesters :: [PubKeyHash]` is the only ClosedDatum
    -- field whose size grows over the head's lifetime. Each Contest adds one
    -- PubKeyHash (28 bytes). With strict == on mustPreserveHeadValue, if
    -- ensureMinCoinTxOut bumps the head output when contesters fills up to
    -- length(parties), Contest would fail.
    --
    -- We don't need to actually run contestTx through a chain; we just
    -- construct a ClosedDatum with all parties listed as contesters and
    -- compare its min-coin against the empty-contesters ClosedDatum's
    -- min-coin. If the worst-case is below the open head's min-coin, we
    -- are safe for the whole head lifecycle.
    let -- baseline: post-coverFee_ value of the open head (from the test above)
        Coin baseline = selectLovelace . txOutValue $ ensureSomeAda (healthyOpenHeadTxOut (mkTxOutDatumInline healthyCurrentOpenDatum))

        -- A ClosedDatum carrying all parties as contesters. We synthesise
        -- 7 fake PubKeyHashes (Hydra's typical max-party count) so the
        -- worst case is independent of healthyParties' size.
        maxParties = 7
        fakeContester n =
          let bs = stringToBuiltinByteString (replicate 28 (chr n))
           in PubKeyHash bs
        worstCloseDatum =
          Head.Closed
            Head.ClosedDatum
              { snapshotNumber = 1
              , parties = healthyOnChainParties
              , contestationDeadline = posixFromUTCTime healthyContestationDeadline
              , contestationPeriod = healthyContestationPeriod
              , headId = toPlutusCurrencySymbol Fixture.testPolicyId
              , contesters = fakeContester <$> [1 .. maxParties]
              , version = toInteger healthyCurrentSnapshotVersion
              , accumulatorCommitment =
                  Accumulator.getAccumulatorCommitment (Accumulator.buildFromSnapshotUTxOs @Tx mempty Nothing Nothing)
              , headAdaOverhead = 0
              }

        worstCloseTxOut =
          (mkHeadOutput Fixture.testNetworkId Fixture.testPolicyId (verificationKeyToOnChainId <$> healthyParticipants) (mkTxOutDatumInline worstCloseDatum))

        Coin worstMinCoin = selectLovelace . txOutValue $ ensureSomeAda worstCloseTxOut

    putStrLn $ "open head min-coin baseline:                       " <> show baseline
    putStrLn $ "ClosedDatum min-coin with " <> show maxParties <> " contesters: " <> show worstMinCoin
    putStrLn $ "delta (worst Closed - open baseline):              " <> show (worstMinCoin - baseline)

    when (worstMinCoin > baseline) $
      expectationFailure $
        "Contest's strict == would fail when contesters fills up: "
          <> "ClosedDatum's min-coin ("
          <> show worstMinCoin
          <> ") exceeds the open head's min-coin ("
          <> show baseline
          <> "). Each Contest grows the contesters list by one PubKeyHash. "
          <> "If coverFee_ bumps the contest output to satisfy the new min-coin, "
          <> "the strict val' == val check in mustPreserveHeadValue would reject the tx."

    worstMinCoin `shouldSatisfy` (<= baseline)
