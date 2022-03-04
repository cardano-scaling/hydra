module Hydra.Cardano.Api.TxBody where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.ExecutionUnits (fromLedgerExUnits)
import Hydra.Cardano.Api.PlutusScript (fromLedgerScript)
import Hydra.Cardano.Api.PolicyId (toLedgerScriptHash)
import Hydra.Cardano.Api.RdmrTag (RdmrTag (..), fromLedgerTag)
import Hydra.Cardano.Api.ScriptData (FromScriptData, fromLedgerData)
import Hydra.Cardano.Api.TxIn (toLedgerTxIn)

import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.Tx as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Data.Map as Map
import Data.Maybe.Strict (strictMaybeToMaybe)
import GHC.Records (HasField (..))
import qualified Plutus.V1.Ledger.Api as Plutus

-- | Find and deserialise from 'ScriptData', a redeemer from the transaction
-- associated to the given input.
findRedeemerSpending ::
  forall a.
  ( FromScriptData a
  ) =>
  Tx AlonzoEra ->
  TxIn ->
  Maybe a
findRedeemerSpending (getTxBody -> ShelleyTxBody _ body _ scriptData _ _) txIn = do
  idx <- strictMaybeToMaybe $ Ledger.indexOf needle haystack
  let ptr = Ledger.RdmrPtr Ledger.Spend idx
  lookupRedeemer ptr scriptData
 where
  needle = toLedgerTxIn txIn
  haystack = getField @"inputs" body

findRedeemerMinting ::
  forall a.
  ( FromScriptData a
  ) =>
  Tx AlonzoEra ->
  PolicyId ->
  Maybe a
findRedeemerMinting (getTxBody -> ShelleyTxBody _ body _ scriptData _ _) policyId = do
  idx <- strictMaybeToMaybe $ Ledger.indexOf needle haystack
  let ptr = Ledger.RdmrPtr Ledger.Mint idx
  lookupRedeemer ptr scriptData
 where
  needle = toLedgerScriptHash policyId
  haystack = getField @"minted" body :: Set (Ledger.ScriptHash StandardCrypto)

findScriptMinting ::
  forall lang.
  () =>
  Tx AlonzoEra ->
  PolicyId ->
  Maybe (PlutusScript lang)
findScriptMinting (getTxBody -> ShelleyTxBody _ body scripts _ _ _) policyId = do
  _idx <- strictMaybeToMaybe $ Ledger.indexOf needle haystack
  fromLedgerScript @_ @lang <$> find ((== needle) . Ledger.hashScript @(ShelleyLedgerEra AlonzoEra)) scripts
 where
  needle = toLedgerScriptHash policyId
  haystack = getField @"minted" body :: Set (Ledger.ScriptHash StandardCrypto)

foldRedeemers ::
  Tx AlonzoEra ->
  result ->
  (RdmrTag -> Word64 -> ScriptData -> ExecutionUnits -> result -> result) ->
  result
foldRedeemers (getTxBody -> ShelleyTxBody _ _ _ scriptData _ _) result0 step =
  Map.foldrWithKey
    ( \(Ledger.RdmrPtr tag ix) (d, ex) ->
        step (fromLedgerTag tag) ix (fromLedgerData d) (fromLedgerExUnits ex)
    )
    result0
    redeemers
 where
  redeemers = case scriptData of
    TxBodyNoScriptData ->
      mempty
    TxBodyScriptData _ _ (Ledger.Redeemers rs) ->
      rs

hasScript ::
  Tx AlonzoEra ->
  PlutusScript lang ->
  Bool
hasScript (getTxBody -> ShelleyTxBody _ _ scripts _ _ _) script =
  script `elem` (fromLedgerScript <$> scripts)

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
