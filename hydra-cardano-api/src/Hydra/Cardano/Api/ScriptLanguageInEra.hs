module Hydra.Cardano.Api.ScriptLanguageInEra where

import Hydra.Cardano.Api.Prelude

-- | Smart-constructor for 'ScriptLanguageInEra' to write functions
-- manipulating scripts that do not commit to a particular era.
class HasScriptLanguage lang era where
  scriptLanguageInEra :: ScriptLanguageInEra lang era

instance HasScriptLanguage PlutusScriptV1 AlonzoEra where
  scriptLanguageInEra = PlutusScriptV1InAlonzo

instance HasScriptLanguage PlutusScriptV2 AlonzoEra where
  scriptLanguageInEra = PlutusScriptV2InAlonzo
