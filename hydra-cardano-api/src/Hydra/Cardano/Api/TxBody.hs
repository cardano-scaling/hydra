module Hydra.Cardano.Api.TxBody where

import Hydra.Cardano.Api.Prelude

import Cardano.Api (toShelleyTxIn)
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Api (
  AsItem (..),
  AsIx,
  ConwayPlutusPurpose (..),
  PlutusPurpose,
 )
import Cardano.Ledger.Babbage.Core (redeemerPointer)
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import Cardano.Ledger.Plutus.Data qualified as Ledger
import Data.Map qualified as Map
import PlutusLedgerApi.V3 qualified as Plutus

-- | Find and deserialise from 'ScriptData', a redeemer from the transaction
-- associated to the given input.
findRedeemerSpending ::
  Plutus.FromData a =>
  Tx Era ->
  TxIn ->
  Maybe a
findRedeemerSpending (getTxBody -> ShelleyTxBody _ body _ scriptData _ _) txIn = do
  ptr <- strictMaybeToMaybe $ redeemerPointer body (ConwaySpending . AsItem $ toShelleyTxIn txIn)
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
