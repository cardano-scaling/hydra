module Hydra.Cardano.Api.ScriptDatum where

import "hydra-cardano-api" Hydra.Cardano.Api.Prelude

import "hydra-cardano-api" Hydra.Cardano.Api.ScriptData (ToScriptData, toScriptData)

-- * Extras

-- | Construct a 'ScriptDatum' for use as transaction witness.
mkScriptDatum :: ToScriptData a => a -> ScriptDatum WitCtxTxIn
mkScriptDatum =
  ScriptDatumForTxIn . Just . toScriptData
