module Hydra.Cardano.Api.ExecutionUnits where

import "cardano-ledger-alonzo" Cardano.Ledger.Alonzo.Scripts qualified as Ledger

import Hydra.Cardano.Api.Prelude

-- * Type Conversions

-- | Convert a cardano-api 'ExecutionUnits' into a cardano-ledger 'ExUnits'
toLedgerExUnits :: ExecutionUnits -> Ledger.ExUnits
toLedgerExUnits = toAlonzoExUnits
