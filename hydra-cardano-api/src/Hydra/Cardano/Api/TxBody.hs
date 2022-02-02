module Hydra.Cardano.Api.TxBody where

import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.ScriptData (FromScriptData)
import Hydra.Cardano.Api.TxIn (toLedgerTxIn)

import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Plutus.V1.Ledger.Api as Plutus

-- | Find and deserialise from 'ScriptData', a redeemer from the transaction
-- associated to the given input.
findRedeemerSpending ::
  FromScriptData a =>
  Tx Era ->
  TxIn ->
  Maybe a
findRedeemerSpending (getTxBody -> ShelleyTxBody _ body _ scriptData _ _) txIn = do
  idx <- Set.lookupIndex (toLedgerTxIn txIn) (Ledger.inputs body)
  let ptr = Ledger.RdmrPtr Ledger.Spend $ fromIntegral idx
  (d, _exUnits) <- Map.lookup ptr redeemers
  Plutus.fromData $ Ledger.getPlutusData d
 where
  redeemers = case scriptData of
    TxBodyNoScriptData ->
      mempty
    TxBodyScriptData _ _ (Ledger.Redeemers rs) ->
      rs
