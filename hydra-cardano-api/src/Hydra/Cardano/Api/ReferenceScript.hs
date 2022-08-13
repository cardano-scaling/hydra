module Hydra.Cardano.Api.ReferenceScript where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.PlutusScript (fromPlutusScript)
import Hydra.Cardano.Api.ReferenceTxInsScriptsInlineDatumsSupportedInEra (HasInlineDatums (..))
import qualified Plutus.V2.Ledger.Api as Plutus

-- | Construct a 'ReferenceScript' from any given Plutus script.
--
-- NOTE: The script is treated as a 'PlutusScriptV2'
mkScriptRef :: Plutus.Script -> ReferenceScript Era
mkScriptRef =
  ReferenceScript inlineDatumsSupportedInEra
    . toScriptInAnyLang
    . PlutusScript PlutusScriptV2
    . fromPlutusScript
