module Hydra.Cardano.Api.ScriptDatum where

import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.ScriptData (ToScriptData, toScriptData)

-- * Extras

-- | Construct a 'ScriptDatum' for use as transaction witness.
mkScriptDatum :: ToScriptData a => a -> ScriptDatum WitCtxTxIn
mkScriptDatum =
  ScriptDatumForTxIn . Just . toScriptData
