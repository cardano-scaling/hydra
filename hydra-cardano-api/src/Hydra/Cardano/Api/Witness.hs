module Hydra.Cardano.Api.Witness where

import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.ScriptLanguageInEra (HasScriptLanguage (..))

-- | Construct a full script witness from a datum, a redeemer and a full
-- 'PlutusScript'. That witness has no execution budget.
mkScriptWitness ::
  forall ctx era lang.
  ( IsPlutusScriptLanguage lang
  , HasScriptLanguage lang era
  ) =>
  PlutusScript lang ->
  ScriptDatum ctx ->
  ScriptRedeemer ->
  ScriptWitness ctx era
mkScriptWitness script datum redeemer =
  PlutusScriptWitness
    (scriptLanguageInEra @lang @era)
    (plutusScriptVersion @lang)
    (PScript script)
    datum
    redeemer
    (ExecutionUnits 0 0)

-- | Construct a reference script witness, only referring to a 'TxIn' which is
-- expected to contain the given script (only required to satisfy types).
mkScriptReference ::
  forall ctx era lang.
  ( IsPlutusScriptLanguage lang
  , HasScriptLanguage lang era
  ) =>
  TxIn ->
  PlutusScript lang ->
  ScriptDatum ctx ->
  ScriptRedeemer ->
  ScriptWitness ctx era
mkScriptReference txIn _script datum redeemer =
  PlutusScriptWitness
    (scriptLanguageInEra @lang @era)
    (plutusScriptVersion @lang)
    (PReferenceScript txIn Nothing)
    datum
    redeemer
    (ExecutionUnits 0 0)
