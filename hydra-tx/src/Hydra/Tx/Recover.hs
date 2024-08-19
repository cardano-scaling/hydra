module Hydra.Tx.Recover where

import Hydra.Prelude

import Hydra.Cardano.Api
import Hydra.Cardano.Api.Network (networkIdToNetwork)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Ledger.Cardano.Builder (
  addInputs,
  addOutputs,
  emptyTxBody,
  setValidityLowerBound,
  unsafeBuildTransaction,
 )
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Tx (HeadId, headIdToCurrencySymbol)

-- | Builds a recover transaction to recover locked funds from the v_deposit script.
recoverTx ::
  NetworkId ->
  HeadId ->
  -- | Deposit input
  TxIn ->
  -- | Already Deposited funds
  [Commit.Commit] ->
  -- | Recover deadline
  UTCTime ->
  -- | Lower bound slot number
  SlotNo ->
  Tx
recoverTx networkId headId depositedTxIn depositted deadline lowerBoundSlot =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs recoverInputs
      & addOutputs deposittedOutputs
      & setValidityLowerBound lowerBoundSlot
 where
  recoverInputs = (,depositWitness) <$> [depositedTxIn]

  redeemer = Deposit.Recover $ fromIntegral $ length deposittedOutputs

  depositWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptWitness depositScript (mkScriptDatum constructedDatum) (toScriptData redeemer)

  constructedDatum = (headIdToCurrencySymbol headId, posixFromUTCTime deadline, depositted)

  deposittedOutputs =
    let deposited = mapMaybe (Commit.deserializeCommit (networkIdToNetwork networkId)) depositted
     in fmap (toTxContext . snd) deposited

  depositScript = fromPlutusScript @PlutusScriptV2 Deposit.validatorScript
