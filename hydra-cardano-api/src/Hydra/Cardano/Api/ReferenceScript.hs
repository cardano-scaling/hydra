module Hydra.Cardano.Api.ReferenceScript where

import Hydra.Cardano.Api.Prelude (Era)

import Cardano.Api (IsPlutusScriptLanguage, PlutusScript, babbageBasedEra, plutusScriptVersion, toScriptInAnyLang)
import Cardano.Api qualified as Api
import Cardano.Api.Shelley (ReferenceScript (..))

-- | Construct a 'ReferenceScript' from any given Plutus script.
mkScriptRef :: IsPlutusScriptLanguage lang => PlutusScript lang -> ReferenceScript Era
mkScriptRef =
  ReferenceScript babbageBasedEra
    . toScriptInAnyLang
    -- Erik TODO: The qualified `Api.PlutusScript` may not be needed when all re-exports are removed.
    . Api.PlutusScript plutusScriptVersion
