module Hydra.Cardano.Api.ReferenceScript where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.PlutusScript (fromPlutusScript)
import PlutusLedgerApi.V3 qualified as Plutus

-- | Construct a 'ReferenceScript' from any given Plutus script.
--
-- NOTE: The script is treated as a 'PlutusScriptV3'
mkScriptRef :: Plutus.SerialisedScript -> ReferenceScript Era
mkScriptRef =
  ReferenceScript babbageBasedEra
    . toScriptInAnyLang
    . PlutusScript PlutusScriptV3
    . fromPlutusScript
