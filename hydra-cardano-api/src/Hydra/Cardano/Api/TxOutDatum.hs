{-# LANGUAGE TypeApplications #-}

module Hydra.Cardano.Api.TxOutDatum where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.ReferenceTxInsScriptsInlineDatumsSupportedInEra (HasInlineDatums (..))
import Hydra.Cardano.Api.ScriptData (ToScriptData, toScriptData)
import Hydra.Cardano.Api.ScriptDataSupportedInEra (HasScriptData (..))

-- | Construct an inline 'TxOutDatum' from some serialisable data.
mkTxOutDatum ::
  forall era a ctx.
  (ToScriptData a, HasInlineDatums era) =>
  a ->
  TxOutDatum ctx era
mkTxOutDatum =
  TxOutDatumInline (inlineDatumsSupportedInEra @era) . toScriptData

-- | Construct a 'TxOutDatum' as a 'ScriptData' hash from some serialisable data.
mkTxOutDatumHash ::
  forall era a ctx.
  (ToScriptData a, HasScriptData era) =>
  a ->
  TxOutDatum ctx era
mkTxOutDatumHash =
  TxOutDatumHash (scriptDataSupportedInEra @era) . hashScriptData . toScriptData
