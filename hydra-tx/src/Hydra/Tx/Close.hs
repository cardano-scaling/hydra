{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Close where

import Hydra.Cardano.Api hiding (utxo)
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Data.ContestationPeriod (addContestationPeriod)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Data.DepositPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger.Cardano.Builder (unsafeBuildTransaction)
import Hydra.Plutus.Extras.Time (posixFromUTCTime, posixToUTCTime)
import Hydra.Tx (
  ConfirmedSnapshot (..),
  HeadId,
  ScriptRegistry (headReference),
  Snapshot (..),
  SnapshotNumber,
  SnapshotVersion,
  commitOutputsHash,
  fromChainSnapshotNumber,
  getSnapshot,
  headIdToCurrencySymbol,
  headReference,
 )
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (toPlutusSignatures)
import Hydra.Tx.IsTx (hashUTxO)
import Hydra.Tx.Utils (IncrementalAction (..), findStateToken, mkHydraHeadV2TxName)
import PlutusLedgerApi.V3 (toBuiltin)

-- * Construction

type PointInTime = (SlotNo, UTCTime)

-- | Representation of the head thread UTxO while the head is in the Open state.
data OpenThreadOutput = OpenThreadOutput
  { openThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , openContestationPeriod :: OnChain.ContestationPeriod
  , openDepositPeriod :: OnChain.DepositPeriod
  , openParties :: [OnChain.Party]
  }
  deriving stock (Eq, Show, Generic)

-- | Create a transaction closing a head with either the initial snapshot or
-- with a multi-signed confirmed snapshot.
closeTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Head identifier
  HeadId ->
  -- | Last known version of the open head.
  SnapshotVersion ->
  -- | Snapshot with instructions how to close the head.
  ConfirmedSnapshot Tx ->
  -- | Lower validity slot number, usually a current or quite recent slot number.
  SlotNo ->
  -- | Upper validity slot and UTC time to compute the contestation deadline time.
  PointInTime ->
  -- | Everything needed to spend the Head state-machine output.
  OpenThreadOutput ->
  IncrementalAction ->
  Tx
closeTx scriptRegistry vk headId openVersion confirmedSnapshot startSlotNo (endSlotNo, utcTime) openThreadOutput incrementalAction =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns [(headInput, headWitness)]
      & addTxInsReference [headScriptRef] mempty
      & addTxOuts [headOutputAfter]
      & addTxExtraKeyWits [verificationKeyHash vk]
      & setTxValidityLowerBound (TxValidityLowerBound startSlotNo)
      & setTxValidityUpperBound (TxValidityUpperBound endSlotNo)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV2TxName "CloseTx")
 where
  OpenThreadOutput
    { openThreadUTxO = (headInput, headOutputBefore)
    , openContestationPeriod
    , openDepositPeriod
    , openParties
    } = openThreadOutput

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum headRedeemer

  headScriptRef =
    fst (headReference scriptRegistry)

  headRedeemer = toScriptData $ Head.Close closeRedeemer

  closeRedeemer =
    case confirmedSnapshot of
      InitialSnapshot{} ->
        Head.CloseInitial
      ConfirmedSnapshot{signatures} ->
        let accHash = toBuiltin $ Accumulator.getAccumulatorHash accumulator
            appliedAccHash = toBuiltin $ Accumulator.getAccumulatorHash appliedAccumulator
            decommitHash = toBuiltin $ hashUTxO @Tx (fromMaybe mempty utxoToDecommit)
            commitHash = toBuiltin $ commitOutputsHash snapshot
            sig = toPlutusSignatures signatures
         in case incrementalAction of
              NoThing ->
                Head.CloseAny{signature = sig, accumulatorHash = accHash, appliedAccumulatorHash = appliedAccHash, decommitOutputsHash = decommitHash, commitOutputsHash = commitHash}
              _ ->
                if version == openVersion
                  then Head.CloseUnused{signature = sig, accumulatorHash = accHash, appliedAccumulatorHash = appliedAccHash, decommitOutputsHash = decommitHash, commitOutputsHash = commitHash}
                  else Head.CloseUsed{signature = sig, accumulatorHash = accHash, appliedAccumulatorHash = appliedAccHash, decommitOutputsHash = decommitHash, commitOutputsHash = commitHash}

  headOutputAfter =
    modifyTxOutDatum (const headDatumAfter) headOutputBefore

  snapshot@Snapshot{number, utxo, utxoToCommit, utxoToDecommit, accumulator, appliedAccumulator, version} = getSnapshot confirmedSnapshot

  -- Whether the snapshot's pending increment/decrement has already been applied
  -- on chain. This decides both which UTxO the head still holds and which of the
  -- two signed accumulators the closed datum must commit to (the validator
  -- enforces the latter by redeemer kind, see 'Hydra.Contract.Head.checkClose').
  pendingActionApplied = version /= openVersion

  -- The UTxO the head still owes at close time: a pending decommit is inside
  -- until its decrement lands, a pending commit only once its increment landed.
  utxoInHead
    | pendingActionApplied = utxo <> fold utxoToCommit
    | otherwise = utxo <> fold utxoToDecommit

  -- The accumulator committing to exactly 'utxoInHead'.
  accumulatorInHead
    | pendingActionApplied = appliedAccumulator
    | otherwise = accumulator

  -- Lovelace in the head UTxO not attributable to any L2 UTxO value (the
  -- min-UTxO overhead). Computed once at Close and propagated unchanged through
  -- Contest and partial fanout steps so the on-chain conservation check can use
  -- strict equality rather than >=.
  headAdaOverhead =
    let Coin headLovelace = selectLovelace (txOutValue headOutputBefore)
        Coin utxoLovelace = selectLovelace (UTxO.totalValue utxoInHead)
     in headLovelace - utxoLovelace

  headDatumAfter =
    mkTxOutDatumInline $
      Head.Closed
        Head.ClosedDatum
          { snapshotNumber = fromIntegral number
          , parties = openParties
          , contestationDeadline
          , contestationPeriod = openContestationPeriod
          , depositPeriod = openDepositPeriod
          , headId = headIdToCurrencySymbol headId
          , contesters = []
          , version = fromIntegral openVersion
          , accumulatorCommitment = Accumulator.getAccumulatorCommitment accumulatorInHead
          , headAdaOverhead
          }

  contestationDeadline =
    addContestationPeriod (posixFromUTCTime utcTime) openContestationPeriod

-- * Observation

data CloseObservation = CloseObservation
  { headId :: HeadId
  , snapshotNumber :: SnapshotNumber
  , contestationDeadline :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Identify a close tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeCloseTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe CloseObservation
observeCloseTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript inputUTxO Head.validatorScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Open Head.OpenDatum{}, Head.Close{}) -> do
      (_, newHeadOutput) <- findTxOutByScript (utxoFromTx tx) Head.validatorScript
      newHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut newHeadOutput
      (closeContestationDeadline, onChainSnapshotNumber) <- case fromScriptData newHeadDatum of
        Just (Head.Closed Head.ClosedDatum{contestationDeadline, snapshotNumber}) ->
          pure (contestationDeadline, snapshotNumber)
        _ -> Nothing
      pure
        CloseObservation
          { headId
          , snapshotNumber = fromChainSnapshotNumber onChainSnapshotNumber
          , contestationDeadline = posixToUTCTime closeContestationDeadline
          }
    _ -> Nothing
