module Hydra.Cardano.Api.ScriptData where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Data.Map as Map
import qualified Plutus.V1.Ledger.Api as Plutus

-- | Data-types that can be marshalled into a generic 'ScriptData' structure.
type ToScriptData a = Plutus.ToData a

-- | Data-types that can be unmarshalled from a generic 'ScriptData' structure.
type FromScriptData a = Plutus.FromData a

-- | Serialise some type into a generic 'ScriptData' structure.
toScriptData :: (ToScriptData a) => a -> ScriptData
toScriptData =
  fromPlutusData . Plutus.toData

{-# DEPRECATED mkRedeemerForTxIn "use 'asScriptData' instead." #-}
mkRedeemerForTxIn :: (ToScriptData a) => a -> ScriptRedeemer
mkRedeemerForTxIn =
  toScriptData

-- | Get the 'ScriptData' associated to the a 'TxOut'. Note that this requires
-- the 'CtxTx' context. To get script data in a 'CtxUTxO' context, see
-- 'lookupScriptData'.
getScriptData :: TxOut CtxTx era -> Maybe ScriptData
getScriptData (TxOut _ _ d) =
  case d of
    TxOutDatum _ dat -> Just dat
    _ -> Nothing

{-# DEPRECATED getDatum "use 'getScriptData' instead." #-}
getDatum :: TxOut CtxTx era -> Maybe ScriptData
getDatum = getScriptData

-- | Lookup included datum of given 'TxOut'.
lookupScriptData :: Tx Era -> TxOut CtxUTxO Era -> Maybe ScriptData
lookupScriptData (Tx (ShelleyTxBody _ _ _ scriptsData _ _) _) = \case
  TxOut _ _ TxOutDatumNone ->
    Nothing
  TxOut _ _ (TxOutDatumHash _ (ScriptDataHash h)) ->
    fromPlutusData . Ledger.getPlutusData <$> Map.lookup h datums
 where
  datums = case scriptsData of
    TxBodyNoScriptData -> mempty
    TxBodyScriptData _ (Ledger.TxDats m) _ -> m

{-# DEPRECATED lookupDatum "use 'lookupScriptData' instead." #-}
lookupDatum :: Tx Era -> TxOut CtxUTxO Era -> Maybe ScriptData
lookupDatum = lookupScriptData
