module Hydra.Cardano.Api.ScriptData where

import Hydra.Cardano.Api.Prelude

import Cardano.Api.Byron (TxBody (..))
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Data.Map as Map
import qualified Plutus.V1.Ledger.Api as Plutus

-- * Extras

-- | Data-types that can be marshalled into a generic 'ScriptData' structure.
type ToScriptData a = Plutus.ToData a

-- | Serialise some type into a generic 'ScriptData' structure.
toScriptData :: (ToScriptData a) => a -> ScriptData
toScriptData =
  fromPlutusData . Plutus.toData

-- | Data-types that can be unmarshalled from a generic 'ScriptData' structure.
type FromScriptData a = Plutus.FromData a

-- | Interpret a script data as some type.
fromScriptData :: (FromScriptData a) => ScriptData -> Maybe a
fromScriptData =
  Plutus.fromData . Ledger.getPlutusData . toLedgerData

-- | Get the 'ScriptData' associated to the a 'TxOut'. Note that this requires
-- the 'CtxTx' context. To get script data in a 'CtxUTxO' context, see
-- 'lookupScriptData'.
getScriptData :: TxOut CtxTx era -> Maybe ScriptData
getScriptData (TxOut _ _ d) =
  case d of
    TxOutDatum _ dat -> Just dat
    _ -> Nothing

-- | Lookup included datum of given 'TxOut'.
lookupScriptData ::
  forall era.
  ( UsesStandardCrypto era
  , Typeable (ShelleyLedgerEra era)
  ) =>
  Tx era ->
  TxOut CtxUTxO era ->
  Maybe ScriptData
lookupScriptData (Tx ByronTxBody{} _) =
  const Nothing
lookupScriptData (Tx (ShelleyTxBody _ _ _ scriptsData _ _) _) = \case
  TxOut _ _ TxOutDatumNone ->
    Nothing
  TxOut _ _ (TxOutDatumHash _ (ScriptDataHash h)) ->
    fromPlutusData . Ledger.getPlutusData <$> Map.lookup h datums
 where
  datums = case scriptsData of
    TxBodyNoScriptData -> mempty
    TxBodyScriptData _ (Ledger.TxDats m) _ -> m

-- * Type Conversions

-- | Convert a cardano-ledger's script 'Data' into a cardano-api's 'ScriptDatum'.
fromLedgerData :: Ledger.Data era -> ScriptData
fromLedgerData =
  fromAlonzoData

-- | Convert a cardano-api's 'ScriptData' into a cardano-ledger's script 'Data'.
toLedgerData :: ScriptData -> Ledger.Data era
toLedgerData =
  toAlonzoData
