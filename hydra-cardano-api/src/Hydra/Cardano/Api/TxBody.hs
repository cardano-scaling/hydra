module Hydra.Cardano.Api.TxBody where

import Hydra.Cardano.Api.Prelude

import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Api (
  AsItem (..),
  AsIx,
  ConwayPlutusPurpose (..),
  PlutusPurpose,
 )
import Cardano.Ledger.Babbage.Core (redeemerPointer)
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Plutus.Data qualified as Ledger
import Data.List (find)
import Data.Map qualified as Map
import Hydra.Cardano.Api.PlutusScript (fromLedgerScript)
import Hydra.Cardano.Api.PolicyId (toLedgerPolicyID, toLedgerScriptHash)
import Hydra.Cardano.Api.TxIn (toLedgerTxIn)
import PlutusLedgerApi.V3 qualified as Plutus

-- | Find and deserialise from 'ScriptData', a redeemer from the transaction
-- associated to the given input.
findRedeemerSpending ::
  Plutus.FromData a =>
  Tx Era ->
  TxIn ->
  Maybe a
findRedeemerSpending (getTxBody -> ShelleyTxBody _ body _ scriptData _ _) txIn = do
  ptr <- strictMaybeToMaybe $ redeemerPointer body (ConwaySpending . AsItem $ toLedgerTxIn txIn)
  lookupRedeemer ptr scriptData

findRedeemerMinting ::
  Plutus.FromData a =>
  Tx Era ->
  PolicyId ->
  Maybe a
findRedeemerMinting (getTxBody -> ShelleyTxBody _ body _ scriptData _ _) pid = do
  ptr <- strictMaybeToMaybe $ redeemerPointer body (ConwayMinting . AsItem $ toLedgerPolicyID pid)
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
  Plutus.FromData a =>
  PlutusPurpose AsIx LedgerEra ->
  TxBodyScriptData Era ->
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
