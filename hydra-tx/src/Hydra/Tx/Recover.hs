module Hydra.Tx.Recover where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
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
import Hydra.Tx (HeadId, mkHeadId)
import Hydra.Tx.Utils (mkHydraHeadV1TxName)

-- | Builds a recover transaction to recover locked funds from the v_deposit script.
recoverTx ::
  -- | Deposit input
  TxIn ->
  -- | Deposited UTxO to recover
  UTxO ->
  -- | Lower bound slot number
  SlotNo ->
  Tx
recoverTx depositTxIn deposited lowerBoundSlot =
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

  depositOutputs =
    toTxContext <$> toList deposited

  depositScript = fromPlutusScript @PlutusScriptV2 Deposit.validatorScript

data RecoverObservation = RecoverObservation
  { headId :: HeadId
  , recoveredUTxO :: UTxO
  }
  deriving stock (Show, Eq, Generic)

observeRecoverTx ::
  NetworkId ->
  UTxO ->
  Tx ->
  Maybe RecoverObservation
observeRecoverTx networkId utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (_, depositOut) <- findTxOutByScript @PlutusScriptV2 inputUTxO depositScript
  dat <- txOutScriptData $ toTxContext depositOut
  Deposit.DepositDatum (headCurrencySymbol, _, onChainDeposits) <- fromScriptData dat
  deposits <- do
    depositedUTxO <- traverse (Commit.deserializeCommit (networkIdToNetwork networkId)) onChainDeposits
    pure $ UTxO.fromPairs depositedUTxO
  headId <- fmap mkHeadId . fromPlutusCurrencySymbol $ headCurrencySymbol
  let depositOuts = toTxContext . snd <$> UTxO.pairs deposits
  -- NOTE: All deposit outputs need to be present in the recover tx outputs but
  -- the two lists of outputs are not necesarilly the same.
  if all (`elem` txOuts' tx) depositOuts
    then
      pure
        ( RecoverObservation
            { headId
            , recoveredUTxO = deposits
            }
        )
    else Nothing
 where
  depositScript = fromPlutusScript Deposit.validatorScript
