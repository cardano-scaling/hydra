{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Regressions for equal-value output substitution during fanout.
--
-- Every attack pays an output which has exactly the value of a live output but
-- a different identity. Consequently value conservation and token burning are
-- valid; only the accumulator membership check can reject it.
module Hydra.Tx.Contract.FanoutSnapshotIdentity where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.List qualified as List
import Hydra.Cardano.Api
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (mkHeadTokenScript)
import Hydra.Tx (ConfirmedSnapshot (..), Snapshot (..), accumulatorInHead, mkHeadId, registryUTxO)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Close (OpenThreadOutput (..), closeTx)
import Hydra.Tx.Contract.PartialFanout qualified as Partial
import Hydra.Tx.Crypto (aggregate, sign)
import Hydra.Tx.Fanout (fanoutTx, finalPartialFanoutTx, partialFanoutTx)
import Hydra.Tx.Utils (IncrementalAction (..))
import PlutusLedgerApi.V3 (toBuiltin)
import Test.Hydra.Tx.Fixture (testPolicyId, testSeedInput)

data FanoutIdentityScenario = FanoutIdentityScenario
  { scenarioName :: String
  , validClose :: (Tx, UTxO)
  , validFullFanout :: (Tx, UTxO)
  , substitutedFullFanout :: (Tx, UTxO)
  , validPartialFromClosed :: (Tx, UTxO)
  , substitutedPartialFromClosed :: (Tx, UTxO)
  , validPartialFromProgress :: (Tx, UTxO)
  , substitutedPartialFromProgress :: (Tx, UTxO)
  , validFinalPartialFanout :: (Tx, UTxO)
  , substitutedFinalPartialFanout :: (Tx, UTxO)
  }

data Settlement = UnabsorbedCommit | SettledDecommit
  deriving stock (Eq)

fanoutIdentityScenarios :: [FanoutIdentityScenario]
fanoutIdentityScenarios =
  [ scenarioFor UnabsorbedCommit
  , scenarioFor SettledDecommit
  ]

-- | Three live outputs let us exercise both partial-fanout transitions before
-- the final step. The first output is the victim replaced by 'excludedUTxO'.
liveUTxO :: UTxO
liveUTxO = UTxO.fromList $ take 3 $ UTxO.toList Partial.healthyFullUTxO

victimUTxO :: UTxO
victimUTxO = UTxO.fromList $ take 1 $ UTxO.toList liveUTxO

firstBatchUTxO :: UTxO
firstBatchUTxO = UTxO.fromList $ take 1 $ drop 1 $ UTxO.toList liveUTxO

lastBatchUTxO :: UTxO
lastBatchUTxO = UTxO.fromList $ take 1 $ drop 2 $ UTxO.toList liveUTxO

nonVictimUTxO :: UTxO
nonVictimUTxO = liveUTxO `UTxO.difference` victimUTxO

-- | An output excluded from the amount the head owes. Its value is changed to
-- exactly equal the victim, while its address and TxIn remain different.
excludedUTxO :: UTxO
excludedUTxO =
  UTxO.map
    (modifyTxOutValue (const $ UTxO.totalValue victimUTxO))
    Partial.presettledUTxO

substitutedFullUTxO :: UTxO
substitutedFullUTxO = nonVictimUTxO <> excludedUTxO

scenarioFor :: Settlement -> FanoutIdentityScenario
scenarioFor settlement =
  FanoutIdentityScenario
    { scenarioName =
        if settlement == SettledDecommit
          then "settled decommit / CloseUsed"
          else "unabsorbed deposit / CloseUnused"
    , validClose = closed
    , validFullFanout = full liveUTxO
    , substitutedFullFanout = full substitutedFullUTxO
    , validPartialFromClosed = firstPartial firstBatchUTxO (victimUTxO <> lastBatchUTxO)
    , substitutedPartialFromClosed = firstPartial excludedUTxO nonVictimUTxO
    , validPartialFromProgress = secondPartial lastBatchUTxO victimUTxO
    , substitutedPartialFromProgress = secondPartial excludedUTxO lastBatchUTxO
    , validFinalPartialFanout = finalPartial victimUTxO
    , substitutedFinalPartialFanout = finalPartial excludedUTxO
    }
 where
  snapshot = snapshotFor settlement
  closed = closeFor settlement snapshot

  full distributed =
    let headInput = outputOf $ fst closed
        tx =
          fromRight (error "fanout identity fixture: cannot build full fanout proof") $
            fanoutTx
              Partial.scriptRegistry
              distributed
              Nothing
              Nothing
              headInput
              Partial.healthySlotNo
              (mkHeadTokenScript testSeedInput)
     in withLookup tx headInput

  partial headInput@(_, headOutput) distributed remaining =
    let tx =
          partialFanoutTx
            Partial.scriptRegistry
            distributed
            headInput
            Partial.healthySlotNo
            (progressDatum headOutput)
            (Accumulator.buildFromUTxO @Tx remaining)
     in withLookup tx headInput

  firstPartial = partial (outputOf $ fst closed)

  -- Use the output of the valid first step so this is a reachable
  -- FanoutProgress -> FanoutProgress transition.
  secondPartial = partial (outputOf $ fst $ firstPartial firstBatchUTxO (victimUTxO <> lastBatchUTxO))

  -- Use the output of both valid partial steps. The attack proof is complete
  -- for the excluded output itself, so completeness and value conservation
  -- pass; membership against the live victim is the rejecting condition.
  finalPartial distributed =
    let headInput =
          outputOf . fst $
            secondPartial lastBatchUTxO victimUTxO
        tx =
          fromRight (error "fanout identity fixture: cannot build final fanout proof") $
            finalPartialFanoutTx
              Partial.scriptRegistry
              distributed
              (Accumulator.buildFromUTxO @Tx distributed)
              headInput
              Partial.healthySlotNo
              (mkHeadTokenScript testSeedInput)
     in withLookup tx headInput

snapshotFor :: Settlement -> Snapshot Tx
snapshotFor settlement =
  Partial.presettledSnapshot
    { utxo = liveUTxO
    , utxoToCommit = pendingCommit
    , utxoToDecommit = pendingDecommit
    , depositTxId =
        if settledDecommit
          then Nothing
          else Just . getTxId . getTxBody . fst $ Partial.presettledCloseTx
    , accumulator
    , appliedAccumulator
    }
 where
  settledDecommit = settlement == SettledDecommit
  pendingCommit = if settledDecommit then Nothing else Just excludedUTxO
  pendingDecommit = if settledDecommit then Just excludedUTxO else Nothing
  (accumulator, appliedAccumulator) =
    Accumulator.buildFromSnapshotUTxOs liveUTxO pendingCommit pendingDecommit

closeFor :: Settlement -> Snapshot Tx -> (Tx, UTxO)
closeFor settlement snapshot = (tx, lookupUTxO)
 where
  settledDecommit = settlement == SettledDecommit
  openVersion = if settledDecommit then 1 else 0
  signedSnapshot =
    ConfirmedSnapshot
      snapshot
      (aggregate [sign sk snapshot | sk <- Partial.healthySigningKeys])

  openOutput =
    Partial.presettledOpenHeadOutput
      & modifyTxOutDatum (const $ mkTxOutDatumInline $ Head.Open openDatum)
      & modifyTxOutValue
        ( <>
            negateValue (UTxO.totalValue Partial.healthyFullUTxO)
              <> UTxO.totalValue liveUTxO
        )

  openDatum =
    case txOutDatum Partial.presettledOpenHeadOutput of
      TxOutDatumInline scriptData
        | Just (Head.Open datum) <- fromScriptData scriptData ->
            datum
              { Head.version = toInteger openVersion
              , Head.accumulatorHash =
                  toBuiltin . Accumulator.getAccumulatorHash $
                    accumulatorInHead openVersion snapshot
              }
      _ -> error "fanout identity fixture: missing Open datum"

  tx =
    closeTx
      Partial.scriptRegistry
      (List.head Partial.healthyParticipants)
      (mkHeadId testPolicyId)
      openVersion
      signedSnapshot
      Partial.presettledCloseLowerBoundSlot
      Partial.presettledCloseUpperBound
      OpenThreadOutput
        { openThreadUTxO = (Partial.presettledOpenHeadInput, openOutput)
        , openContestationPeriod = Partial.healthyContestationPeriod
        , openDepositPeriod = openDatum.depositPeriod
        , openParties = openDatum.parties
        }
      (if settledDecommit then ToDecommit else ToCommit)

  lookupUTxO =
    UTxO.singleton Partial.presettledOpenHeadInput openOutput
      <> registryUTxO Partial.scriptRegistry

outputOf :: Tx -> (TxIn, TxOut CtxUTxO)
outputOf tx =
  case txOuts' tx of
    headOutput : _ -> (mkTxIn tx 0, toCtxUTxOTxOut headOutput)
    [] -> error "fanout identity fixture: transaction has no head output"

progressDatum :: TxOut CtxUTxO -> Head.FanoutProgressDatum
progressDatum output =
  case txOutDatum output of
    TxOutDatumInline scriptData ->
      case fromScriptData scriptData of
        Just (Head.Closed datum) -> Head.progressFromClosed datum
        Just (Head.FanoutProgress datum) -> datum
        _ -> error "fanout identity fixture: wrong head state"
    _ -> error "fanout identity fixture: missing head datum"

withLookup :: Tx -> (TxIn, TxOut CtxUTxO) -> (Tx, UTxO)
withLookup tx (headInput, headOutput) =
  ( tx
  , UTxO.singleton headInput headOutput
      <> registryUTxO Partial.scriptRegistry
  )
