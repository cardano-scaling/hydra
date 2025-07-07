module Hydra.Cardano.Api.ExecutionUnits where

import Hydra.Cardano.Api.Prelude
    ( ExecutionUnits, toAlonzoExUnits )

import Cardano.Ledger.Alonzo.Scripts qualified as Ledger

-- * Type Conversions

-- | Convert a cardano-api 'ExecutionUnits' into a cardano-ledger 'ExUnits'
toLedgerExUnits :: ExecutionUnits -> Ledger.ExUnits
toLedgerExUnits = toAlonzoExUnits
