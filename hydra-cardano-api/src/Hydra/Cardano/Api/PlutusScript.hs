{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.PlutusScript where

import Hydra.Cardano.Api.Prelude

import PlutusLedgerApi.Common qualified as Plutus

-- * Type Conversions

-- | Convert a serialized plutus script into a cardano-api 'PlutusScript'.
fromPlutusScript :: Plutus.SerialisedScript -> PlutusScript lang
fromPlutusScript =
  PlutusScriptSerialised
