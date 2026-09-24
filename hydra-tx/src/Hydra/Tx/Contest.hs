module Hydra.Tx.Contest where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Data.ContestationPeriod (addContestationPeriod)
import Hydra.Data.DepositPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger.Cardano.Builder (unsafeBuildTransaction)
import Hydra.Plutus.Extras (posixToUTCTime)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Close (PointInTime)
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toChain)
import Hydra.Tx.Crypto (MultiSignature (..), fromPlutusSignatures, toPlutusSignatures)
import Hydra.Tx.HeadId (HeadId, headIdToCurrencySymbol)
import Hydra.Tx.ScriptRegistry (ScriptRegistry, headReference)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion, accumulatorInHead, commitOutputsHash, decommitOutputsHash, fromChainSnapshotNumber, pendingActionApplied)
import Hydra.Tx.Utils (findStateToken, mkHydraHeadV2TxName)
import PlutusLedgerApi.V1.Crypto qualified as Plutus
import PlutusLedgerApi.V3 (toBuiltin)
import PlutusLedgerApi.V3 qualified as Plutus

import Hydra.Plutus.Orphans ()

-- * Construction

data ClosedThreadOutput = ClosedThreadOutput
  { closedThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , closedParties :: [OnChain.Party]
  , closedContestationDeadline :: Plutus.POSIXTime
  , closedContesters :: [Plutus.PubKeyHash]
  , closedHeadAdaOverhead :: Integer
  , closedDepositPeriod :: OnChain.DepositPeriod
  }
  deriving stock (Eq, Show, Generic)

-- XXX: This function is VERY similar to the 'closeTx' function (only notable
-- difference being the redeemer, which is in itself also the same structure as
-- the close's one. We could potentially refactor this to avoid repetition or do
-- something more principled at the protocol level itself and "merge" close and
-- contest as one operation.
contestTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  HeadId ->
  ContestationPeriod ->
  SnapshotVersion ->
  -- | Contested snapshot number (i.e. the one we contest to)
  Snapshot Tx ->
  -- | Multi-signature of the whole snapshot
  MultiSignature (Snapshot Tx) ->
  -- | Current slot and posix time to be used as the contestation time.
  PointInTime ->
  -- | Everything needed to spend the Head state-machine output.
  ClosedThreadOutput ->
  Tx
contestTx scriptRegistry vk headId contestationPeriod openVersion snapshot sig (slotNo, _) closedThreadOutput =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns [(headInput, headWitness)]
      & addTxInsReference [headScriptRef] mempty
      & addTxOuts [headOutputAfter]
      & addTxExtraKeyWits [verificationKeyHash vk]
      & setTxValidityUpperBound (TxValidityUpperBound slotNo)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV2TxName "ContestTx")
 where
  Snapshot{number, accumulator, appliedAccumulator} = snapshot

  ClosedThreadOutput
    { closedThreadUTxO = (headInput, headOutputBefore)
    , closedParties
    , closedContestationDeadline
    , closedContesters
    , closedHeadAdaOverhead
    , closedDepositPeriod
    } = closedThreadOutput

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum headRedeemer

  headScriptRef =
    fst (headReference scriptRegistry)

  accHash = toBuiltin $ Accumulator.getAccumulatorHash accumulator

  appliedAccHash = toBuiltin $ Accumulator.getAccumulatorHash appliedAccumulator

  decommitHash = toBuiltin $ decommitOutputsHash snapshot

  commitHash = toBuiltin $ commitOutputsHash snapshot

  contestRedeemer
    | pendingActionApplied openVersion snapshot =
        Head.ContestUsed{signature = toPlutusSignatures sig, accumulatorHash = accHash, appliedAccumulatorHash = appliedAccHash, decommitOutputsHash = decommitHash, commitOutputsHash = commitHash}
    | otherwise =
        Head.ContestUnused{signature = toPlutusSignatures sig, accumulatorHash = accHash, appliedAccumulatorHash = appliedAccHash, decommitOutputsHash = decommitHash, commitOutputsHash = commitHash}

  headRedeemer = toScriptData $ Head.Contest contestRedeemer

  headOutputAfter =
    modifyTxOutDatum (const headDatumAfter) headOutputBefore

  contester = toPlutusKeyHash (verificationKeyHash vk)

  onChainConstestationPeriod = toChain contestationPeriod

  newContestationDeadline =
    if length (contester : closedContesters) == length closedParties
      then closedContestationDeadline
      else addContestationPeriod closedContestationDeadline onChainConstestationPeriod

  headDatumAfter =
    mkTxOutDatumInline $
      Head.Closed
        Head.ClosedDatum
          { snapshotNumber = toInteger number
          , parties = closedParties
          , contestationDeadline = newContestationDeadline
          , contestationPeriod = onChainConstestationPeriod
          , depositPeriod = closedDepositPeriod
          , headId = headIdToCurrencySymbol headId
          , contesters = contester : closedContesters
          , version = toInteger openVersion
          , accumulatorCommitment = Accumulator.getAccumulatorCommitment (accumulatorInHead openVersion snapshot)
          , headAdaOverhead = closedHeadAdaOverhead
          }

-- * Observation

data ContestObservation = ContestObservation
  { headId :: HeadId
  , snapshotNumber :: SnapshotNumber
  , contestationDeadline :: UTCTime
  , contesters :: [Plutus.PubKeyHash]
  , signatures :: MultiSignature (Snapshot Tx)
  -- ^ Multisignature of the contesting snapshot.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Identify a contest tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeContestTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe ContestObservation
observeContestTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript inputUTxO Head.validatorScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Closed Head.ClosedDatum{}, Head.Contest contestRedeemer) -> do
      (_, newHeadOutput) <- findTxOutByScript (utxoFromTx tx) Head.validatorScript
      newHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut newHeadOutput
      (onChainSnapshotNumber, contestationDeadline, contesters) <- decodeDatum newHeadDatum
      pure
        ContestObservation
          { headId
          , snapshotNumber = fromChainSnapshotNumber onChainSnapshotNumber
          , contestationDeadline = posixToUTCTime contestationDeadline
          , contesters
          , signatures = contestSignatures contestRedeemer
          }
    _ -> Nothing
 where
  -- NOTE: A decode failure must not drop the observation; empty signatures
  -- never verify, so HeadLogic just skips adoption.
  contestSignatures :: Head.ContestRedeemer -> MultiSignature (Snapshot Tx)
  contestSignatures = \case
    Head.ContestUnused{signature} -> decodeSignatures signature
    Head.ContestUsed{signature} -> decodeSignatures signature

  decodeSignatures :: [Head.Signature] -> MultiSignature (Snapshot Tx)
  decodeSignatures = fromMaybe mempty . fromPlutusSignatures

  -- NOTE: The head validator constrains the produced datum of a contest, so a
  -- different state here should be unreachable. Observation runs on
  -- attacker-supplied transactions on the chain-sync thread, though, so this
  -- declines to observe rather than throwing, like every sibling observer.
  decodeDatum headDatum =
    case fromScriptData headDatum of
      Just (Head.Closed Head.ClosedDatum{snapshotNumber, contestationDeadline, contesters}) ->
        Just (snapshotNumber, contestationDeadline, contesters)
      _ -> Nothing
