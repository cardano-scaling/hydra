module Hydra.Tx.Fanout where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Hydra.Contract.Head (emptyHash)
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.MintAction (MintAction (..))
import Hydra.Ledger.Cardano.Builder (burnTokens, unsafeBuildTransaction)
import Hydra.Tx.Accumulator (HydraAccumulator)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.HeadId (HeadId)
import Hydra.Tx.ScriptRegistry (ScriptRegistry (..))
import Hydra.Tx.Utils (findStateToken, headTokensFromValue, mkHydraHeadV1TxName)
import PlutusLedgerApi.V3 (toBuiltin)
import PlutusTx.Builtins (bls12_381_G2_compressed_generator, bls12_381_G2_uncompress)

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
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  -- | Minting Policy script, made from initial seed
  PlutusScript ->
  Tx
fanoutTx scriptRegistry utxo utxoToCommit utxoToDecommit (headInput, headOutput) deadlineSlotNo headTokenScript =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns [(headInput, headWitness)]
      & addTxInsReference [headScriptRef, crsScriptRef] mempty
      & addTxOuts (orderedTxOutsToFanout <> orderedTxOutsToCommit <> orderedTxOutsToDecommit)
      & burnTokens headTokenScript Burn headTokens
      & setTxValidityLowerBound (TxValidityLowerBound $ deadlineSlotNo + 1)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "FanoutTx")
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
        , crsRef = toPlutusTxOutRef crsScriptRef
        }

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
-- and continues the Closed state with an updated accumulator.
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
  -- | The closed datum from the current head output
  Head.ClosedDatum ->
  -- | Remaining accumulator after removing the distributed subset
  HydraAccumulator ->
  Tx
partialFanoutTx scriptRegistry utxoToDistribute (headInput, headOutput) deadlineSlotNo closedDatum remainingAccumulator =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns [(headInput, headWitness)]
      & addTxInsReference [headScriptRef, crsScriptRef] mempty
      & addTxOuts (headOutputAfter : orderedDistributedOutputs)
      & setTxValidityLowerBound (TxValidityLowerBound $ deadlineSlotNo + 1)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "PartialFanoutTx")
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

  -- Continuing head output with updated ClosedDatum
  headOutputAfter =
    modifyTxOutDatum (const headDatumAfter) headOutput

  headDatumAfter =
    mkTxOutDatumInline $
      Head.Closed
        closedDatum
          { Head.utxoHash = emptyHash
          , Head.alphaUTxOHash = emptyHash
          , Head.omegaUTxOHash = emptyHash
          , Head.accumulatorHash = toBuiltin remainingAccumulatorHash
          , Head.accumulatorCommitment = remainingAccumulatorCommitment
          , Head.proof = bls12_381_G2_uncompress bls12_381_G2_compressed_generator
          }

  remainingAccumulatorHash = Accumulator.getAccumulatorHash remainingAccumulator
  remainingAccumulatorCommitment = Accumulator.getAccumulatorCommitment remainingAccumulator

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
