module Hydra.Cardano.Api.ReferenceScript where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.PlutusScript (fromPlutusScript)
import PlutusLedgerApi.V2 qualified as Plutus

-- | Construct a 'ReferenceScript' from any given Plutus script.
--
-- NOTE: The script is treated as a 'PlutusScriptV2'
mkScriptRef :: Plutus.SerialisedScript -> ReferenceScript Era
mkScriptRef =
  ReferenceScript babbageBasedEra
    . toScriptInAnyLang
    . PlutusScript PlutusScriptV2
    . fromPlutusScript
