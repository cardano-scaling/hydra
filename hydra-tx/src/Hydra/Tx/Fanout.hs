module Hydra.Tx.Fanout where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.MintAction (MintAction (..))
import Hydra.Ledger.Cardano.Builder (
  addInputs,
  addOutputs,
  addReferenceInputs,
  burnTokens,
  emptyTxBody,
  setValidityLowerBound,
  unsafeBuildTransaction,
 )
import Hydra.Tx.ScriptRegistry (ScriptRegistry (..))
import Hydra.Tx.Utils (headTokensFromValue, mkHydraHeadV1TxName)

-- | Create the fanout transaction, which distributes the closed state
-- accordingly. The head validator allows fanout only > deadline, so we need
-- to set the lower bound to be deadline + 1 slot.
fanoutTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Snapshotted UTxO to fanout on layer 1
  UTxO ->
  -- | Snapshotted decommit UTxO to fanout on layer 1
  Maybe UTxO ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  -- | Minting Policy script, made from initial seed
  PlutusScript ->
  Tx
fanoutTx scriptRegistry utxo utxoToDecommit (headInput, headOutput) deadlineSlotNo headTokenScript =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addReferenceInputs [headScriptRef]
      & addOutputs (orderedTxOutsToFanout <> orderedTxOutsToDecommit)
      & burnTokens headTokenScript Burn headTokens
      & setValidityLowerBound (deadlineSlotNo + 1)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "FanoutTx")
 where
  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer
  headScriptRef =
    fst (headReference scriptRegistry)
  headScript =
    fromPlutusScript @PlutusScriptV3 Head.validatorScript
  headRedeemer =
    toScriptData $
      Head.Fanout
        { numberOfFanoutOutputs = fromIntegral $ length utxo
        , numberOfDecommitOutputs = fromIntegral $ maybe 0 length utxoToDecommit
        }

  headTokens =
    headTokensFromValue headTokenScript (txOutValue headOutput)

  orderedTxOutsToFanout =
    toTxContext <$> toList utxo

  orderedTxOutsToDecommit =
    case utxoToDecommit of
      Nothing -> []
      Just decommitUTxO -> toTxContext <$> toList decommitUTxO
