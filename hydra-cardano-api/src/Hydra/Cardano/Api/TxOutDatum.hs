module Hydra.Cardano.Api.TxOutDatum where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.ScriptData (ToScriptData, toScriptData)

-- | Construct a 'TxOutDatum' to be included in the tx from some serialisable data.
mkTxOutDatum ::
  forall era a.
  (ToScriptData a, IsAlonzoEraOnwards era) =>
  a ->
  TxOutDatum CtxTx era
mkTxOutDatum =
  TxOutDatumInTx (alonzoEraOnwards @era) . toScriptData

-- | Construct a 'TxOutDatum' as a 'ScriptData' hash from some serialisable data.
mkTxOutDatumHash ::
  forall era a ctx.
  (ToScriptData a, IsAlonzoEraOnwards era) =>
  a ->
  TxOutDatum ctx era
mkTxOutDatumHash =
  TxOutDatumHash (alonzoEraOnwards @era) . hashScriptDataBytes . toScriptData

-- | Construct an inline 'TxOutDatum' from some serialisable data.
mkTxOutDatumInline ::
  forall era a ctx.
  (ToScriptData a, IsBabbageEraOnwards era) =>
  a ->
  TxOutDatum ctx era
mkTxOutDatumInline =
  TxOutDatumInline (babbageEraOnwards @era) . toScriptData
