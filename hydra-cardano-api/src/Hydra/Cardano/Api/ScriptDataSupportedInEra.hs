module Hydra.Cardano.Api.ScriptDataSupportedInEra where

import Hydra.Cardano.Api.Prelude

-- | Smart-constructor for 'ScriptDataSupportedInEra' to write functions
-- manipulating scripts that do not commit to a particular era.
class HasScriptData era where
  scriptDataSupportedInEra :: AlonzoEraOnwards era

instance HasScriptData AlonzoEra where
  scriptDataSupportedInEra = AlonzoEraOnwardsAlonzo

instance HasScriptData BabbageEra where
  scriptDataSupportedInEra = AlonzoEraOnwardsBabbage
