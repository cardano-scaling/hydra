module Hydra.Tx.Fanout where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
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
  -- | Full snapshot accumulator matching accumulatorCommitment in the closed datum.
  HydraAccumulator ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  -- | Minting Policy script, made from initial seed
  PlutusScript ->
  Tx
fanoutTx scriptRegistry utxo utxoToCommit utxoToDecommit snapshotAccumulator (headInput, headOutput) deadlineSlotNo headTokenScript =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns [(headInput, headWitness)]
      & addTxInsReference [headScriptRef, crsScriptRef] mempty
      & addTxOuts (orderedTxOutsToFanout <> orderedTxOutsToCommit <> orderedTxOutsToDecommit)
      & burnTokens headTokenScript Burn headTokens
      & setTxValidityLowerBound (TxValidityLowerBound $ deadlineSlotNo + 1)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV2TxName "FanoutTx")
 where
  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum headRedeemer

  headScriptRef =
    fst (headReference scriptRegistry)

  crsScriptRef =
    fst (crsReference scriptRegistry)

  utxoLength = UTxO.size utxo

  toCommitLength = length orderedTxOutsToCommit

  toDecommitLength = length orderedTxOutsToDecommit

  headRedeemer =
    toScriptData $
      Head.Fanout
        { numberOfFanoutOutputs = fromIntegral utxoLength
        , numberOfCommitOutputs = fromIntegral toCommitLength
        , numberOfDecommitOutputs = fromIntegral toDecommitLength
        , subsetTxInRefs = subsetTxInRefs
        , proof = fanoutProof
        , crsRef = toPlutusTxOutRef crsScriptRef
        }

  -- Snapshot TxOutRefs in the same order as their corresponding output bodies
  -- below. The on-chain validator zips this with the outputs to compute the
  -- subset scalars; keep the orderings in lockstep.
  subsetTxInRefs =
    (toPlutusTxOutRef <$> orderedTxInsToFanout)
      <> (toPlutusTxOutRef <$> orderedTxInsToCommit)
      <> (toPlutusTxOutRef <$> orderedTxInsToDecommit)

  fanoutProof =
    let allUTxO = utxo <> fromMaybe mempty utxoToCommit <> fromMaybe mempty utxoToDecommit
        -- Use the full snapshot accumulator (same one used for accumulatorCommitment in the
        -- closed datum). CRS must be sized for the full accumulator, not just the fanout subset.
        crs = Accumulator.crsG1Points $ Accumulator.requiredCRSPointCount snapshotAccumulator
     in bls12_381_G1_uncompress $
          toBuiltin $
            Accumulator.createMembershipProofFromUTxO @Tx allUTxO snapshotAccumulator crs

  headTokens =
    headTokensFromValue headTokenScript (txOutValue headOutput)

  orderedTxInsToFanout = fst <$> UTxO.toList utxo
  orderedTxOutsToFanout =
    fromCtxUTxOTxOut <$> UTxO.txOutputs utxo

  (orderedTxInsToCommit, orderedTxOutsToCommit) =
    case utxoToCommit of
      Nothing -> ([], [])
      Just commitUTxO ->
        (fst <$> UTxO.toList commitUTxO, fromCtxUTxOTxOut <$> UTxO.txOutputs commitUTxO)

  (orderedTxInsToDecommit, orderedTxOutsToDecommit) =
    case utxoToDecommit of
      Nothing -> ([], [])
      Just decommitUTxO ->
        (fst <$> UTxO.toList decommitUTxO, fromCtxUTxOTxOut <$> UTxO.txOutputs decommitUTxO)

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
        , subsetTxInRefs = toPlutusTxOutRef . fst <$> UTxO.toList utxoToDistribute
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
    } = progressDatum

  headDatumAfter =
    mkTxOutDatumInline $
      Head.FanoutProgress
        Head.FanoutProgressDatum
          { Head.headId
          , Head.parties
          , Head.contestationDeadline
          , Head.accumulatorCommitment = Accumulator.getAccumulatorCommitment remainingAccumulator
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
  -- | Remaining accumulator (should cover exactly utxoToDistribute)
  HydraAccumulator ->
  Tx
finalPartialFanoutTx scriptRegistry utxoToDistribute (headInput, headOutput) deadlineSlotNo headTokenScript remainingAccumulator =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns [(headInput, headWitness)]
      & addTxInsReference [headScriptRef, crsScriptRef] mempty
      & addTxOuts orderedDistributedOutputs
      & burnTokens headTokenScript Burn headTokens
      & setTxValidityLowerBound (TxValidityLowerBound $ deadlineSlotNo + 1)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV2TxName "FinalPartialFanoutTx")
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
      Head.FinalPartialFanout
        { numberOfPartialOutputs = fromIntegral (UTxO.size utxoToDistribute)
        , subsetTxInRefs = toPlutusTxOutRef . fst <$> UTxO.toList utxoToDistribute
        , proof = fanoutProof
        , crsRef = toPlutusTxOutRef crsScriptRef
        }

  fanoutProof =
    let crs = Accumulator.crsG1Points $ Accumulator.requiredCRSPointCount remainingAccumulator
     in bls12_381_G1_uncompress $
          toBuiltin $
            Accumulator.createMembershipProofFromUTxO @Tx utxoToDistribute remainingAccumulator crs

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
      Head.Fanout{numberOfFanoutOutputs, numberOfCommitOutputs, numberOfDecommitOutputs} -> do
        let allOutputs = fromIntegral $ numberOfFanoutOutputs + numberOfCommitOutputs + numberOfDecommitOutputs
        let fanoutUTxO = UTxO.fromList $ zip (mkTxIn tx <$> [0 ..]) (toCtxUTxOTxOut <$> take allOutputs (txOuts' tx))
        pure FanoutObservation{headId, fanoutUTxO}
      _ -> Nothing

data PartialFanoutObservation = PartialFanoutObservation
  { headId :: HeadId
  , distributedUTxO :: UTxO
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
        let distributedOutputs = take numDistributed (drop 1 (txOuts' tx))
        -- TxIn indices start at 0 for the continuing output, so distributed are at [1..n]
        let distributedUTxO = UTxO.fromList $ zip (mkTxIn tx <$> [1 ..]) (toCtxUTxOTxOut <$> distributedOutputs)
        pure PartialFanoutObservation{headId, distributedUTxO}
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
