{-# LANGUAGE TypeApplications #-}

module Hydra.Cardano.Api.TxOutDatum where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.ReferenceTxInsScriptsInlineDatumsSupportedInEra (HasInlineDatums (..))
import Hydra.Cardano.Api.ScriptData (ToScriptData, toScriptData)
import Hydra.Cardano.Api.ScriptDataSupportedInEra (HasScriptData (..))

-- | Construct a 'TxOutDatum' to be included in the tx from some serialisable data.
mkTxOutDatum ::
  forall era a.
  (ToScriptData a, HasScriptData era) =>
  a ->
  TxOutDatum CtxTx era
mkTxOutDatum =
  TxOutDatumInTx (scriptDataSupportedInEra @era) . toScriptData

-- | Construct a 'TxOutDatum' as a 'ScriptData' hash from some serialisable data.
mkTxOutDatumHash ::
  forall era a ctx.
  (ToScriptData a, HasScriptData era) =>
  a ->
  TxOutDatum ctx era
mkTxOutDatumHash =
  TxOutDatumHash (scriptDataSupportedInEra @era) . hashScriptData . toScriptData

-- | Construct an inline 'TxOutDatum' from some serialisable data.
mkTxOutDatumInline ::
  forall era a ctx.
  (ToScriptData a, HasInlineDatums era) =>
  a ->
  TxOutDatum ctx era
mkTxOutDatumInline =
  TxOutDatumInline (inlineDatumsSupportedInEra @era) . toScriptData
