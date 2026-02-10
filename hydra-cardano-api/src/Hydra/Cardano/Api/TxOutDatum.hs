module Hydra.Cardano.Api.TxOutDatum where

import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.ScriptData (ToScriptData, toScriptData)

-- | Construct a 'TxOutDatum' as a 'ScriptData' hash from some serialisable data.
mkTxOutDatumHash ::
  forall era a ctx.
  (ToScriptData a, IsAlonzoBasedEra era) =>
  a ->
  TxOutDatum ctx era
mkTxOutDatumHash =
  TxOutDatumHash (alonzoBasedEra @era) . hashScriptDataBytes . toScriptData

-- | Construct an inline 'TxOutDatum' from some serialisable data.
mkTxOutDatumInline ::
  forall era a ctx.
  (ToScriptData a, IsBabbageBasedEra era) =>
  a ->
  TxOutDatum ctx era
mkTxOutDatumInline =
  TxOutDatumInline (babbageBasedEra @era) . toScriptData
