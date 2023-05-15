module Hydra.Cardano.Api.TxBody where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.PlutusScript (fromLedgerScript)
import Hydra.Cardano.Api.PolicyId (toLedgerScriptHash)
import Hydra.Cardano.Api.ScriptData (FromScriptData)
import Hydra.Cardano.Api.TxIn (toLedgerTxIn)
import Hydra.Cardano.Api.Value (toLedgerPolicyID)

import qualified Cardano.Ledger.Alonzo.Scripts.Data as Ledger
import qualified Cardano.Ledger.Alonzo.TxWits as Ledger
import qualified Cardano.Ledger.Babbage.Tx as Ledger
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Cardano.Ledger.Core as Ledger
import Data.List (find)
import qualified Data.Map as Map
import qualified PlutusLedgerApi.V2 as Plutus

-- | Find and deserialise from 'ScriptData', a redeemer from the transaction
-- associated to the given input.
findRedeemerSpending ::
  ( FromScriptData a
  ) =>
  Tx Era ->
  TxIn ->
  Maybe a
findRedeemerSpending (getTxBody -> ShelleyTxBody _ body _ scriptData _ _) txIn = do
  ptr <- strictMaybeToMaybe $ Ledger.rdptr body (Ledger.Spending $ toLedgerTxIn txIn)
  lookupRedeemer ptr scriptData

findRedeemerMinting ::
  forall a.
  ( FromScriptData a
  ) =>
  Tx Era ->
  PolicyId ->
  Maybe a
findRedeemerMinting (getTxBody -> ShelleyTxBody _ body _ scriptData _ _) pid = do
  ptr <- strictMaybeToMaybe $ Ledger.rdptr body (Ledger.Minting $ toLedgerPolicyID pid)
  lookupRedeemer ptr scriptData

findScriptMinting ::
  forall lang.
  () =>
  Tx Era ->
  PolicyId ->
  Maybe (PlutusScript lang)
findScriptMinting (getTxBody -> ShelleyTxBody _ _ scripts _ _ _) pid = do
  fromLedgerScript @_ @lang
    <$> find ((== needle) . Ledger.hashScript @(ShelleyLedgerEra Era)) scripts
 where
  needle = toLedgerScriptHash pid

--
-- Internals
--

lookupRedeemer ::
  forall a era.
  ( FromScriptData a
  , Ledger.Era (ShelleyLedgerEra era)
  ) =>
  Ledger.RdmrPtr ->
  TxBodyScriptData era ->
  Maybe a
lookupRedeemer ptr scriptData = do
  (d, _exUnits) <- Map.lookup ptr redeemers
  Plutus.fromData $ Ledger.getPlutusData d
 where
  redeemers = case scriptData of
    TxBodyNoScriptData ->
      mempty
    TxBodyScriptData _ _ (Ledger.Redeemers rs) ->
      rs
