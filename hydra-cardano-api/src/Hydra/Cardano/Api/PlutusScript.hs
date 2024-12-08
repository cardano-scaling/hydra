{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.PlutusScript where

import Hydra.Cardano.Api.Prelude

import Data.ByteString.Short qualified as SBS
import PlutusLedgerApi.Common qualified as Plutus
import Test.QuickCheck (listOf)

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

instance Arbitrary (PlutusScript lang) where
  arbitrary =
    PlutusScriptSerialised . SBS.pack <$> listOf arbitrary
