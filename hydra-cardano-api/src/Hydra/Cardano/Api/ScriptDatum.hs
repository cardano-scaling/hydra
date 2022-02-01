module Hydra.Cardano.Api.ScriptDatum where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.ScriptData (ToScriptData, asScriptData)

-- | Construct a 'ScriptDatum' for use as transaction witness.
mkScriptDatum :: (ToScriptData a) => a -> ScriptDatum WitCtxTxIn
mkScriptDatum =
  ScriptDatumForTxIn . asScriptData

{-# DEPRECATED mkDatumForTxIn "use 'mkScriptDatum' instead." #-}
mkDatumForTxIn :: (ToScriptData a) => a -> ScriptDatum WitCtxTxIn
mkDatumForTxIn =
  mkScriptDatum
