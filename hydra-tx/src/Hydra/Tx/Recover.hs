module Hydra.Tx.Recover where

import Hydra.Prelude

import Hydra.Cardano.Api
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Ledger.Cardano.Builder (
  addInputs,
  addOutputs,
  emptyTxBody,
  setValidityLowerBound,
  unsafeBuildTransaction,
 )
import Hydra.Tx.Utils (mkHydraHeadV1TxName)

-- | Builds a recover transaction to recover locked funds from the v_deposit script.
recoverTx ::
  NetworkId ->
  -- | Deposit input
  TxIn ->
  -- | Already Deposited funds
  [Commit.Commit] ->
  -- | Lower bound slot number
  SlotNo ->
  Tx
recoverTx networkId depositTxIn deposited lowerBoundSlot =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs recoverInputs
      & addOutputs depositOutputs
      & setValidityLowerBound lowerBoundSlot
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "RecoverTx")
 where
  recoverInputs = (,depositWitness) <$> [depositTxIn]

  redeemer = toScriptData $ Deposit.Recover $ fromIntegral $ length depositOutputs

  depositWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptWitness depositScript InlineScriptDatum redeemer

  depositOutputs = toTxContext . snd <$> mapMaybe (Commit.deserializeCommit (networkIdToNetwork networkId)) deposited

  depositScript = fromPlutusScript @PlutusScriptV2 Deposit.validatorScript
