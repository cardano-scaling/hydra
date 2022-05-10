module Hydra.Cardano.Api.PlutusScript where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.Language as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import Codec.Serialise (serialise)
import Hydra.Cardano.Api.PlutusScriptVersion (HasPlutusScriptVersion (..))
import qualified Plutus.V1.Ledger.Api as Plutus

-- * Type Conversions

-- | Convert a cardano-ledger's 'Script' into a cardano-api's 'PlutusScript'
--
-- Note that this function is unsafe in two manners:
--
-- (a) If the given script is a timelock script, it throws an impure exception;
-- (b) If the given script is in a wrong language, it silently coerces it.
fromLedgerScript :: HasCallStack => Ledger.Script era -> PlutusScript lang
fromLedgerScript = \case
  Ledger.TimelockScript{} -> error "fromLedgerScript: TimelockScript"
  Ledger.PlutusScript _ bytes -> PlutusScriptSerialised bytes

-- | Convert a cardano-api's 'PlutusScript' into a cardano-ledger's 'Script'.
toLedgerScript ::
  forall lang.
  (HasPlutusScriptVersion lang) =>
  PlutusScript lang ->
  Ledger.Script (ShelleyLedgerEra Era)
toLedgerScript (PlutusScriptSerialised bytes) =
  let lang = case plutusScriptVersion $ proxyToAsType (Proxy @lang) of
        PlutusScriptV1 -> Ledger.PlutusV1
        PlutusScriptV2 -> Ledger.PlutusV2
   in Ledger.PlutusScript lang bytes

-- | Convert a plutus' 'Script' into a cardano-api's 'PlutusScript'
fromPlutusScript :: Plutus.Script -> PlutusScript lang
fromPlutusScript =
  PlutusScriptSerialised . toShort . fromLazy . serialise
