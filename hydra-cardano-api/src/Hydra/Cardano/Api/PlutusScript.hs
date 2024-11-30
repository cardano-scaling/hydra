{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.PlutusScript where

import Hydra.Cardano.Api.Prelude

import PlutusLedgerApi.Common qualified as Plutus

-- * Type Conversions

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
