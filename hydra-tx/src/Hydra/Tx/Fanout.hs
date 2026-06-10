module Hydra.Tx.Fanout where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Set qualified as Set
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.MintAction (MintAction (..))
import Hydra.Ledger.Cardano.Builder (burnTokens, unsafeBuildTransaction)
import Hydra.Tx.Accumulator (HydraAccumulator)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.HeadId (HeadId)
import Hydra.Tx.ScriptRegistry (ScriptRegistry (..))
import Hydra.Tx.Utils (findStateToken, headTokensFromValue, mkHydraHeadV2TxName)
import PlutusLedgerApi.V3 (toBuiltin)
import PlutusTx.Builtins (bls12_381_G1_uncompress)

-- * Creation

-- | Create the fanout transaction, which distributes the closed state
-- accordingly. The head validator allows fanout only > deadline, so we need
-- to set the lower bound to be deadline + 1 slot.
fanoutTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Snapshotted UTxO to fanout on layer 1
  UTxO ->
  -- | Snapshotted commit UTxO to fanout on layer 1
  Maybe UTxO ->
  -- | Snapshotted decommit UTxO to fanout on layer 1
  Maybe UTxO ->
  -- | Full snapshot UTxO (utxo <> commit <> decommit) used to rebuild the accumulator
  -- matching the closed datum. May differ from the fanned-out outputs when an
  -- incremental action was already applied on-chain before close.
  UTxO ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  -- | Minting Policy script, made from initial seed
  PlutusScript ->
  Either Text Tx
fanoutTx scriptRegistry utxo utxoToCommit utxoToDecommit utxoForProof (headInput, headOutput) deadlineSlotNo headTokenScript = do
  fanoutProof <- computeFanoutProof
  pure $
    unsafeBuildTransaction $
      defaultTxBodyContent
        & addTxIns [(headInput, headWitness fanoutProof)]
        & addTxInsReference [headScriptRef, crsScriptRef] mempty
        & addTxOuts (orderedTxOutsToFanout <> orderedTxOutsToCommit <> orderedTxOutsToDecommit)
        & burnTokens headTokenScript Burn headTokens
        & setTxValidityLowerBound (TxValidityLowerBound $ deadlineSlotNo + 1)
        & setTxMetadata (TxMetadataInEra $ mkHydraHeadV2TxName "FanoutTx")
 where
  headWitness proof =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum (headRedeemer proof)

  headScriptRef =
    fst (headReference scriptRegistry)

  crsScriptRef =
    fst (crsReference scriptRegistry)

  accumulator =
    Accumulator.buildFromUTxO @Tx utxoForProof

  headRedeemer proof =
    toScriptData $
      Head.Fanout
        { numberOfFanoutOutputs = fromIntegral (UTxO.size utxo + maybe 0 UTxO.size utxoToCommit + maybe 0 UTxO.size utxoToDecommit)
        , proof = proof
        , crsRef = toPlutusTxOutRef crsScriptRef
        }

  computeFanoutProof = do
    let subsetUTxO = utxo <> fold utxoToCommit <> fold utxoToDecommit
        crs = Accumulator.crsG1Points $ Accumulator.requiredCRSPointCount accumulator
    proofBytes <- Accumulator.createMembershipProofFromUTxO @Tx subsetUTxO accumulator crs
    pure $ bls12_381_G1_uncompress $ toBuiltin proofBytes

  headTokens =
    headTokensFromValue headTokenScript (txOutValue headOutput)

  orderedTxOutsToFanout =
    fromCtxUTxOTxOut <$> UTxO.txOutputs utxo

  orderedTxOutsToCommit =
    case utxoToCommit of
      Nothing -> []
      Just commitUTxO -> fromCtxUTxOTxOut <$> UTxO.txOutputs commitUTxO

  orderedTxOutsToDecommit =
    case utxoToDecommit of
      Nothing -> []
      Just decommitUTxO -> fromCtxUTxOTxOut <$> UTxO.txOutputs decommitUTxO

-- | Create a partial fanout transaction that distributes a subset of UTxOs
-- and produces a 'FanoutProgress' head output with an updated accumulator.
-- Handles both Closed → FanoutProgress (first step) and FanoutProgress →
-- FanoutProgress (subsequent steps).
--
-- The continuing head output is the first output, followed by the distributed
-- UTxOs. No tokens are burned (that happens on the final full fanout).
partialFanoutTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Subset of UTxOs to distribute in this partial fanout
  UTxO ->
  -- | Head state-machine output to spend
  (TxIn, TxOut CtxUTxO) ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  -- | FanoutProgressDatum from the current head output (the caller converts ClosedDatum if needed)
  Head.FanoutProgressDatum ->
  -- | Remaining accumulator after removing the distributed subset
  HydraAccumulator ->
  Tx
partialFanoutTx scriptRegistry utxoToDistribute (headInput, headOutput) deadlineSlotNo progressDatum remainingAccumulator =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns [(headInput, headWitness)]
      & addTxInsReference [headScriptRef, crsScriptRef] mempty
      & addTxOuts (headOutputAfter : orderedDistributedOutputs)
      & setTxValidityLowerBound (TxValidityLowerBound $ deadlineSlotNo + 1)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV2TxName "PartialFanoutTx")
 where
  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum headRedeemer

  headScriptRef =
    fst (headReference scriptRegistry)

  crsScriptRef =
    fst (crsReference scriptRegistry)

  headRedeemer =
    toScriptData $
      Head.PartialFanout
        { numberOfPartialOutputs = fromIntegral (UTxO.size utxoToDistribute)
        , crsRef = toPlutusTxOutRef crsScriptRef
        }

  -- Continuing head output with FanoutProgressDatum and reduced value.
  -- The head output value is reduced by the sum of distributed output values,
  -- satisfying the on-chain mustConserveValue check:
  --   headInValue == headOutValue <> foldMap txOutValue distributedOutputs
  headOutputAfter =
    modifyTxOutDatum (const headDatumAfter) $
      modifyTxOutValue (<> negateValue distributedValue) headOutput

  distributedValue = UTxO.totalValue utxoToDistribute

  Head.FanoutProgressDatum
    { Head.headId
    , Head.parties
    , Head.contestationDeadline
    , Head.headAdaOverhead
    } = progressDatum

  headDatumAfter =
    mkTxOutDatumInline $
      Head.FanoutProgress
        Head.FanoutProgressDatum
          { Head.headId
          , Head.parties
          , Head.contestationDeadline
          , Head.accumulatorCommitment = Accumulator.getAccumulatorCommitment remainingAccumulator
          , Head.headAdaOverhead
          }

  orderedDistributedOutputs =
    fromCtxUTxOTxOut <$> UTxO.txOutputs utxoToDistribute

-- | Create the final partial fanout transaction that distributes all remaining
-- UTxOs and burns all head tokens. Transitions FanoutProgress → Final.
finalPartialFanoutTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | All remaining UTxOs to distribute in this final fanout
  UTxO ->
  -- | Head state-machine output to spend
  (TxIn, TxOut CtxUTxO) ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  -- | Minting Policy script, made from initial seed
  PlutusScript ->
  Either Text Tx
finalPartialFanoutTx scriptRegistry utxoToDistribute (headInput, headOutput) deadlineSlotNo headTokenScript = do
  fanoutProof <- computeFanoutProof
  pure $
    unsafeBuildTransaction $
      defaultTxBodyContent
        & addTxIns [(headInput, headWitness fanoutProof)]
        & addTxInsReference [headScriptRef, crsScriptRef] mempty
        & addTxOuts orderedDistributedOutputs
        & burnTokens headTokenScript Burn headTokens
        & setTxValidityLowerBound (TxValidityLowerBound $ deadlineSlotNo + 1)
        & setTxMetadata (TxMetadataInEra $ mkHydraHeadV2TxName "FinalPartialFanoutTx")
 where
  headWitness proof =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum (headRedeemer proof)

  headScriptRef =
    fst (headReference scriptRegistry)

  crsScriptRef =
    fst (crsReference scriptRegistry)

  headRedeemer proof =
    toScriptData $
      Head.FinalPartialFanout
        { numberOfPartialOutputs = fromIntegral (UTxO.size utxoToDistribute)
        , proof = proof
        , crsRef = toPlutusTxOutRef crsScriptRef
        }

  remainingAccumulator = Accumulator.buildFromUTxO @Tx utxoToDistribute

  computeFanoutProof = do
    let crs = Accumulator.crsG1Points $ Accumulator.requiredCRSPointCount remainingAccumulator
    proofBytes <- Accumulator.createMembershipProofFromUTxO @Tx utxoToDistribute remainingAccumulator crs
    pure $ bls12_381_G1_uncompress $ toBuiltin proofBytes

  headTokens =
    headTokensFromValue headTokenScript (txOutValue headOutput)

  orderedDistributedOutputs =
    fromCtxUTxOTxOut <$> UTxO.txOutputs utxoToDistribute

-- * Observation

data FanoutObservation = FanoutObservation
  { headId :: HeadId
  , fanoutUTxO :: UTxO
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Identify a fanout tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeFanoutTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe FanoutObservation
observeFanoutTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript inputUTxO Head.validatorScript
  headId <- findStateToken headOutput
  findRedeemerSpending tx headInput
    >>= \case
      Head.Fanout{} -> do
        let fanoutUTxO = UTxO.fromList $ zip (mkTxIn tx <$> [0 ..]) (toCtxUTxOTxOut <$> txOuts' tx)
        pure FanoutObservation{headId, fanoutUTxO}
      _ -> Nothing

data PartialFanoutObservation = PartialFanoutObservation
  { headId :: HeadId
  , distributedOutputs :: Set (TxOut CtxUTxO)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Identify a partial fanout tx by looking up the input spending the Head
-- output and decoding its redeemer as PartialFanout.
observePartialFanoutTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe PartialFanoutObservation
observePartialFanoutTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript inputUTxO Head.validatorScript
  headId <- findStateToken headOutput
  findRedeemerSpending tx headInput
    >>= \case
      Head.PartialFanout{numberOfPartialOutputs} -> do
        -- First output is the continuing head output, distributed outputs follow
        let numDistributed = fromIntegral numberOfPartialOutputs
        let distributedOutputs = Set.fromList (toCtxUTxOTxOut <$> take numDistributed (drop 1 (txOuts' tx)))
        pure PartialFanoutObservation{headId, distributedOutputs}
      _ -> Nothing

-- | Identify a final partial fanout tx by looking up the input spending the Head
-- output and decoding its redeemer as FinalPartialFanout.
-- Returns a FanoutObservation since this is the finalizing step.
observeFinalPartialFanoutTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe FanoutObservation
observeFinalPartialFanoutTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript inputUTxO Head.validatorScript
  headId <- findStateToken headOutput
  findRedeemerSpending tx headInput
    >>= \case
      Head.FinalPartialFanout{numberOfPartialOutputs} -> do
        let numDistributed = fromIntegral numberOfPartialOutputs
        let fanoutUTxO = UTxO.fromList $ zip (mkTxIn tx <$> [0 ..]) (toCtxUTxOTxOut <$> take numDistributed (txOuts' tx))
        pure FanoutObservation{headId, fanoutUTxO}
      _ -> Nothing
