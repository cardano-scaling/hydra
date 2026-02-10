module Hydra.Cardano.Api.ExecutionUnits where

import "hydra-cardano-api" Hydra.Cardano.Api.Prelude

import "cardano-ledger-alonzo" Cardano.Ledger.Alonzo.Scripts qualified as Ledger

-- * Type Conversions

-- | Convert a cardano-api 'ExecutionUnits' into a cardano-ledger 'ExUnits'
toLedgerExUnits :: ExecutionUnits -> Ledger.ExUnits
toLedgerExUnits = toAlonzoExUnits
