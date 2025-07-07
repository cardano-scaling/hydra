module Hydra.Cardano.Api.ReferenceScript where

import Hydra.Cardano.Api.Prelude
    ( toScriptInAnyLang,
      IsBabbageBasedEra(babbageBasedEra),
      IsPlutusScriptLanguage(..),
      PlutusScript,
      Script(PlutusScript),
      ReferenceScript(ReferenceScript),
      Era )

-- | Construct a 'ReferenceScript' from any given Plutus script.
mkScriptRef :: IsPlutusScriptLanguage lang => PlutusScript lang -> ReferenceScript Era
mkScriptRef =
  ReferenceScript babbageBasedEra
    . toScriptInAnyLang
    . PlutusScript plutusScriptVersion
