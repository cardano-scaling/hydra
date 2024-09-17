module Hydra.Tx.Deposit where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Hydra.Cardano.Api
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Ledger.Cardano.Builder (
  addInputs,
  addOutputs,
  emptyTxBody,
  unsafeBuildTransaction,
 )
import Hydra.Plutus.Extras.Time (posixFromUTCTime)
import Hydra.Tx (HeadId, headIdToCurrencySymbol)
import Hydra.Tx.Utils (mkHydraHeadV1TxName)

-- | Builds a deposit transaction to lock funds into the v_deposit script.
depositTx ::
  NetworkId ->
  HeadId ->
  -- | UTxO to deposit
  UTxO ->
  -- | Deposit deadline
  UTCTime ->
  Tx
depositTx networkId headId depositUTxO deadline =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs depositInputs
      & addOutputs [depositOutput]
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "DepositTx")
 where
  depositInputsList = toList (UTxO.inputSet depositUTxO)

  depositInputs = (,BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> depositInputsList

  depositValue = foldMap txOutValue depositUTxO

  depositScript = fromPlutusScript @PlutusScriptV2 Deposit.validatorScript

  deposits = mapMaybe Commit.serializeCommit $ UTxO.pairs depositUTxO

  depositPlutusDatum = Deposit.datum $ Deposit.DepositDatum (headIdToCurrencySymbol headId, posixFromUTCTime deadline, deposits)

  depositDatum = mkTxOutDatumInline depositPlutusDatum

  depositOutput =
    TxOut
      (mkScriptAddress @PlutusScriptV2 networkId depositScript)
      depositValue
      depositDatum
      ReferenceScriptNone
