module Hydra.Cardano.Api.Witness where

import Hydra.Cardano.Api.PlutusScriptVersion (HasPlutusScriptVersion (..))
import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.ScriptLanguageInEra (HasScriptLanguage (..))

-- | Construct a default script witness from a datum, a redeemer and a full
-- 'PlutusScript'. That witness has no execution budget.
mkScriptWitness ::
  forall ctx era lang.
  ( HasPlutusScriptVersion lang
  , HasScriptLanguage lang era
  ) =>
  PlutusScript lang ->
  ScriptDatum ctx ->
  ScriptRedeemer ->
  ScriptWitness ctx era
mkScriptWitness script datum redeemer =
  PlutusScriptWitness
    (scriptLanguageInEra @lang @era)
    (plutusScriptVersion (proxyToAsType (Proxy @lang)))
    script
    datum
    redeemer
    (ExecutionUnits 0 0)
