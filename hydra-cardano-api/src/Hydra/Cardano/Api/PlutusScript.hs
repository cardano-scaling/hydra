{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.PlutusScript where

import Hydra.Cardano.Api.Prelude

import Cardano.Ledger.Alonzo.Scripts qualified as Ledger
import Cardano.Ledger.Plutus.Language qualified as Ledger
import Data.ByteString.Short qualified as SBS
import PlutusLedgerApi.Common qualified as Plutus
import Test.QuickCheck (listOf)

-- * Type Conversions

-- | Convert a cardano-ledger 'Script' into a cardano-api 'PlutusScript'
--
-- NOTE: This function is unsafe in two manners:
--
-- (a) If the given script is a timelock script, it throws an impure exception;
-- (b) If the given script is in a wrong language, it silently coerces it.
fromLedgerScript :: HasCallStack => Ledger.AlonzoScript era -> PlutusScript lang
fromLedgerScript = \case
  Ledger.TimelockScript{} -> error "fromLedgerScript: TimelockScript"
  Ledger.PlutusScript (Ledger.Plutus _ (Ledger.BinaryPlutus bytes)) -> PlutusScriptSerialised bytes

-- | Convert a cardano-api 'PlutusScript' into a cardano-ledger 'Script'.
toLedgerScript ::
  forall lang.
  IsPlutusScriptLanguage lang =>
  PlutusScript lang ->
  Ledger.AlonzoScript (ShelleyLedgerEra Era)
toLedgerScript (PlutusScriptSerialised bytes) =
  let lang = case plutusScriptVersion @lang of
        PlutusScriptV1 -> Ledger.PlutusV1
        PlutusScriptV2 -> Ledger.PlutusV2
        PlutusScriptV3 -> Ledger.PlutusV3
   in Ledger.PlutusScript $ Ledger.Plutus lang (Ledger.BinaryPlutus bytes)

-- | Convert a serialized plutus script into a cardano-api 'PlutusScript'.
fromPlutusScript :: Plutus.SerialisedScript -> PlutusScript lang
fromPlutusScript =
  PlutusScriptSerialised

-- * Orphans

instance IsPlutusScriptLanguage lang => ToJSON (PlutusScript lang) where
  toJSON = toJSON . serialiseToTextEnvelope Nothing

instance IsPlutusScriptLanguage lang => FromJSON (PlutusScript lang) where
  parseJSON v = do
    env <- parseJSON v
    case deserialiseFromTextEnvelope (proxyToAsType Proxy) env of
      Left e -> fail $ show e
      Right a -> pure a

instance Arbitrary (PlutusScript lang) where
  arbitrary =
    PlutusScriptSerialised . SBS.pack <$> listOf arbitrary
