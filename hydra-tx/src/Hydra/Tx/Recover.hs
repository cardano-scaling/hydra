module Hydra.Tx.Recover where

import "hydra-prelude" Hydra.Prelude

import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "hydra-cardano-api" Hydra.Cardano.Api
import "hydra-plutus" Hydra.Contract.Commit qualified as Commit
import "hydra-plutus" Hydra.Contract.Deposit qualified as Deposit
import "hydra-plutus" Hydra.Plutus (depositValidatorScript)
import "hydra-tx" Hydra.Ledger.Cardano.Builder (
  unsafeBuildTransaction,
 )
import "hydra-tx" Hydra.Tx (HeadId, mkHeadId)
import "hydra-tx" Hydra.Tx.Utils (mkHydraHeadV1TxName)

-- | Builds a recover transaction to recover locked funds from the v_deposit script.
recoverTx ::
  -- | Deposit input
  TxId ->
  -- | Deposited UTxO to recover
  UTxO ->
  -- | Lower bound slot number
  SlotNo ->
  Tx
recoverTx depositTxId deposited lowerBoundSlot =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns recoverInputs
      & addTxOuts depositOutputs
      & setTxValidityLowerBound (TxValidityLowerBound lowerBoundSlot)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "RecoverTx")
 where
  recoverInputs = (,depositWitness) <$> [TxIn depositTxId (TxIx 0)]

  redeemer = toScriptData $ Deposit.redeemer $ Deposit.Recover $ fromIntegral $ length depositOutputs

  depositWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptWitness depositValidatorScript InlineScriptDatum redeemer

  depositOutputs =
    fromCtxUTxOTxOut <$> UTxO.txOutputs deposited

data RecoverObservation = RecoverObservation
  { headId :: HeadId
  , recoveredTxId :: TxId
  , recoveredUTxO :: UTxO
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

observeRecoverTx ::
  NetworkId ->
  UTxO ->
  Tx ->
  Maybe RecoverObservation
observeRecoverTx networkId utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (TxIn depositTxId _, depositOut) <- findTxOutByScript inputUTxO depositValidatorScript
  dat <- txOutScriptData $ fromCtxUTxOTxOut depositOut
  (headCurrencySymbol, _, onChainDeposits) <- fromScriptData dat :: Maybe Deposit.DepositDatum
  deposits <- do
    depositedUTxO <- traverse (Commit.deserializeCommit (toShelleyNetwork networkId)) onChainDeposits
    pure $ UTxO.fromList depositedUTxO
  headId <- fmap mkHeadId . fromPlutusCurrencySymbol $ headCurrencySymbol
  let depositOuts = fromCtxUTxOTxOut . snd <$> UTxO.toList deposits
  -- NOTE: All deposit outputs need to be present in the recover tx outputs but
  -- the two lists of outputs are not necesarilly the same.
  if all (`elem` txOuts' tx) depositOuts
    then
      pure
        ( RecoverObservation
            { headId
            , recoveredTxId = depositTxId
            , recoveredUTxO = deposits
            }
        )
    else Nothing
