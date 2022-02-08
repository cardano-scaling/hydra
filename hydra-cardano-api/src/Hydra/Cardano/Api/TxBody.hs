module Hydra.Cardano.Api.TxBody where

import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.ScriptData (FromScriptData)
import Hydra.Cardano.Api.TxIn (toLedgerTxIn)

import Cardano.Api.Byron (TxBody (..))
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Cardano.Ledger.Core as Ledger.Core
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Records (HasField (..))
import qualified Plutus.V1.Ledger.Api as Plutus

-- | Find and deserialise from 'ScriptData', a redeemer from the transaction
-- associated to the given input.
findRedeemerSpending ::
  forall a era.
  ( FromScriptData a
  , Ledger.Era (ShelleyLedgerEra era)
  , HasField
      "inputs"
      (Ledger.Core.TxBody (ShelleyLedgerEra era))
      (Set (Ledger.TxIn StandardCrypto))
  ) =>
  Tx era ->
  TxIn ->
  Maybe a
findRedeemerSpending (getTxBody -> ByronTxBody{}) _ = do
  Nothing
findRedeemerSpending (getTxBody -> ShelleyTxBody _ body _ scriptData _ _) txIn = do
  idx <- Set.lookupIndex (toLedgerTxIn txIn) (getField @"inputs" body)
  let ptr = Ledger.RdmrPtr Ledger.Spend $ fromIntegral idx
  (d, _exUnits) <- Map.lookup ptr redeemers
  Plutus.fromData $ Ledger.getPlutusData d
 where
  redeemers = case scriptData of
    TxBodyNoScriptData ->
      mempty
    TxBodyScriptData _ _ (Ledger.Redeemers rs) ->
      rs
