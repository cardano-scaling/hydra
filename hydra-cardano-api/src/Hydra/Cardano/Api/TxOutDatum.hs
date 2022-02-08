module Hydra.Cardano.Api.TxOutDatum where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.ScriptData (ToScriptData, toScriptData)
import Hydra.Cardano.Api.ScriptDataSupportedInEra (HasScriptData (..))

-- | Construct a 'TxOutDatum' as plain 'ScriptData' from some serialisable data
-- type. Note that this is only possible in 'CtxTx' for 'CtxUTxO' only allows to
-- embed hashes.
mkTxOutDatum ::
  forall era a.
  (ToScriptData a, HasScriptData era) =>
  a ->
  TxOutDatum CtxTx era
mkTxOutDatum =
  TxOutDatum (scriptDataSupportedInEra @era) . toScriptData

-- | Construct a 'TxOutDatum' as a 'ScriptData' hash, from some serialisable
-- data type.
mkTxOutDatumHash ::
  forall era a ctx.
  (ToScriptData a, HasScriptData era) =>
  a ->
  TxOutDatum ctx era
mkTxOutDatumHash =
  TxOutDatumHash (scriptDataSupportedInEra @era) . hashScriptData . toScriptData
