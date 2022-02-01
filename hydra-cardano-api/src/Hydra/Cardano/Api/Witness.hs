module Hydra.Cardano.Api.Witness where

import Hydra.Cardano.Api.IsScriptWitnessInCtx (IsScriptWitnessInCtx (..))
import Hydra.Cardano.Api.Prelude

-- | Construct a default script witness from a datum, a redeemer and a full
-- 'PlutusScript'. That witness has no execution budget.
mkScriptWitness ::
  forall ctx.
  (IsScriptWitnessInCtx ctx) =>
  PlutusScript PlutusScriptV1 ->
  ScriptDatum ctx ->
  ScriptRedeemer ->
  Witness ctx Era
mkScriptWitness script datum redeemer =
  ScriptWitness scriptWitnessCtx witness
 where
  witness =
    PlutusScriptWitness
      PlutusScriptV1InAlonzo
      PlutusScriptV1
      script
      datum
      redeemer
      (ExecutionUnits 0 0)
