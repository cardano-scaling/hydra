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
  accumulatorInHead,
  commitOutputsHash,
  decommitOutputsHash,
  fromChainSnapshotNumber,
  getSnapshot,
  hasPendingAction,
  headIdToCurrencySymbol,
  headReference,
  pendingActionApplied,
 )
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (MultiSignature, observedSignatures, toPlutusSignatures)
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

-- | Whether a head at the given open version can be closed with the snapshot,
-- such that the closed head can later be fanned out.
--
-- The head validator only accepts a snapshot signed at the open version
-- ('Head.CloseInitial', 'Head.CloseAny', 'Head.CloseUnused') or one before
-- ('Head.CloseUsed'). The latter stores the snapshot's applied accumulator,
-- which only matches the head value if the version bump was this snapshot's
-- own increment or decrement. For a snapshot without a pending action the
-- version moved on through another snapshot's settlement, so the closed head
-- could never be fanned out: a later snapshot this node has not adopted, or an
-- earlier commit this snapshot dropped once its increment could no longer
-- land, which landed after all. A settling decommit is never dropped (the
-- snapshot request handler requires every request at its version to carry
-- it), so a confirmed snapshot never lags the head by a decommit it already
-- accounts for.
isClosableAt :: SnapshotVersion -> Snapshot tx -> Bool
isClosableAt openVersion snapshot@Snapshot{version} =
  version == openVersion
    || (version + 1 == openVersion && hasPendingAction snapshot)

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
            decommitHash = toBuiltin $ decommitOutputsHash snapshot
            commitHash = toBuiltin $ commitOutputsHash snapshot
            sig = toPlutusSignatures signatures
         in if pendingActionApplied openVersion snapshot
              then
                Head.CloseUsed{signature = sig, accumulatorHash = accHash, appliedAccumulatorHash = appliedAccHash, decommitOutputsHash = decommitHash, commitOutputsHash = commitHash}
              else case incrementalAction of
                NoThing ->
                  Head.CloseAny{signature = sig, accumulatorHash = accHash, appliedAccumulatorHash = appliedAccHash, decommitOutputsHash = decommitHash, commitOutputsHash = commitHash}
                _ ->
                  Head.CloseUnused{signature = sig, accumulatorHash = accHash, appliedAccumulatorHash = appliedAccHash, decommitOutputsHash = decommitHash, commitOutputsHash = commitHash}

  headOutputAfter =
    modifyTxOutDatum (const headDatumAfter) headOutputBefore

  snapshot@Snapshot{number, utxo, utxoToCommit, utxoToDecommit, accumulator, appliedAccumulator} = getSnapshot confirmedSnapshot

  -- What the head holds at close time: a pending decommit is inside until its
  -- decrement happened, a pending commit only once its increment happened.
  utxoInHead
    | pendingActionApplied openVersion snapshot = utxo <> fold utxoToCommit
    | otherwise = utxo <> fold utxoToDecommit

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
          , accumulatorCommitment = Accumulator.getAccumulatorCommitment (accumulatorInHead openVersion snapshot)
          , headAdaOverhead
          }

  contestationDeadline =
    addContestationPeriod (posixFromUTCTime utcTime) openContestationPeriod

-- * Observation

data CloseObservation = CloseObservation
  { headId :: HeadId
  , snapshotNumber :: SnapshotNumber
  , contestationDeadline :: UTCTime
  , signatures :: MultiSignature (Snapshot Tx)
  -- ^ Multisignature of the closing snapshot, empty for the initial snapshot.
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
    (Head.Open Head.OpenDatum{}, Head.Close closeRedeemer) -> do
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
          , signatures = closeSignatures closeRedeemer
          }
    _ -> Nothing
 where
  closeSignatures :: Head.CloseRedeemer -> MultiSignature (Snapshot Tx)
  closeSignatures = \case
    Head.CloseInitial -> mempty
    Head.CloseAny{signature} -> observedSignatures signature
    Head.CloseUnused{signature} -> observedSignatures signature
    Head.CloseUsed{signature} -> observedSignatures signature
