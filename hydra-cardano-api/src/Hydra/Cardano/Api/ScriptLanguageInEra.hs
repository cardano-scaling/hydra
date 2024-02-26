module Hydra.Cardano.Api.ScriptLanguageInEra where

import Hydra.Cardano.Api.Prelude

-- | Smart-constructor for 'ScriptLanguageInEra' to write functions
-- manipulating scripts that do not commit to a particular era.
class HasScriptLanguage lang era where
  scriptLanguageInEra :: ScriptLanguageInEra lang era

instance HasScriptLanguage PlutusScriptV1 AlonzoEra where
  scriptLanguageInEra = PlutusScriptV1InAlonzo

instance HasScriptLanguage PlutusScriptV1 BabbageEra where
  scriptLanguageInEra = PlutusScriptV1InBabbage

instance HasScriptLanguage PlutusScriptV2 BabbageEra where
  scriptLanguageInEra = PlutusScriptV2InBabbage

instance HasScriptLanguage PlutusScriptV1 ConwayEra where
  scriptLanguageInEra = PlutusScriptV1InConway

instance HasScriptLanguage PlutusScriptV2 ConwayEra where
  scriptLanguageInEra = PlutusScriptV2InConway

instance HasScriptLanguage PlutusScriptV3 ConwayEra where
  scriptLanguageInEra = PlutusScriptV3InConway
