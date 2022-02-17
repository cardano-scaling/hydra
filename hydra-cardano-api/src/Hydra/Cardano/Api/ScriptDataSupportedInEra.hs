module Hydra.Cardano.Api.ScriptDataSupportedInEra where

import Hydra.Cardano.Api.Prelude

-- | Smart-constructor for 'ScriptDataSupportedInEra' to write functions
-- manipulating scripts that do not commit to a particular era.
class IsShelleyBasedEra era => HasScriptData era where
  scriptDataSupportedInEra :: ScriptDataSupportedInEra era

instance HasScriptData AlonzoEra where
  scriptDataSupportedInEra = ScriptDataInAlonzoEra
