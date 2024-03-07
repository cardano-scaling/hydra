module Cardano.Api.Class.HasScriptLanguageInEra where

import Cardano.Api (AlonzoEra, BabbageEra, ConwayEra, PlutusScriptV1, PlutusScriptV2, PlutusScriptV3, ScriptLanguageInEra (..))

-- | Smart-constructor for 'ScriptLanguageInEra' to write functions
-- manipulating scripts that do not commit to a particular era.
class HasScriptLanguageInEra lang era where
  scriptLanguageInEra :: ScriptLanguageInEra lang era

instance HasScriptLanguageInEra PlutusScriptV1 AlonzoEra where
  scriptLanguageInEra = PlutusScriptV1InAlonzo

instance HasScriptLanguageInEra PlutusScriptV1 BabbageEra where
  scriptLanguageInEra = PlutusScriptV1InBabbage

instance HasScriptLanguageInEra PlutusScriptV2 BabbageEra where
  scriptLanguageInEra = PlutusScriptV2InBabbage

instance HasScriptLanguageInEra PlutusScriptV1 ConwayEra where
  scriptLanguageInEra = PlutusScriptV1InConway

instance HasScriptLanguageInEra PlutusScriptV2 ConwayEra where
  scriptLanguageInEra = PlutusScriptV2InConway

instance HasScriptLanguageInEra PlutusScriptV3 ConwayEra where
  scriptLanguageInEra = PlutusScriptV3InConway
