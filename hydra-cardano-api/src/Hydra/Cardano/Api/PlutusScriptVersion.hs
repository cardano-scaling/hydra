module Hydra.Cardano.Api.PlutusScriptVersion where

import Hydra.Cardano.Api.Prelude

-- | Reify a type-level script version into a value-level 'PlutusScriptVersion'
class HasTypeProxy lang => HasPlutusScriptVersion lang where
  plutusScriptVersion :: AsType lang -> PlutusScriptVersion lang

instance HasPlutusScriptVersion PlutusScriptV1 where
  plutusScriptVersion _ = PlutusScriptV1

instance HasPlutusScriptVersion PlutusScriptV2 where
  plutusScriptVersion _ = PlutusScriptV2
