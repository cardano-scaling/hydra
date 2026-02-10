module Hydra.Cardano.Api.TxBody where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.TxIn (toLedgerTxIn)
import "cardano-ledger-alonzo" Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import "cardano-ledger-api" Cardano.Ledger.Api (
  AsItem (..),
  AsIx,
  ConwayPlutusPurpose (..),
  PlutusPurpose,
 )
import "cardano-ledger-babbage" Cardano.Ledger.Babbage.Core (redeemerPointer)
import "cardano-ledger-core" Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import "cardano-ledger-core" Cardano.Ledger.Plutus.Data qualified as Ledger
import "containers" Data.Map qualified as Map
import "plutus-ledger-api" PlutusLedgerApi.V3 qualified as Plutus

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
