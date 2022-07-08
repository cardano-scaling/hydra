module Hydra.Cardano.Api.ExecutionUnits where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.Scripts as Ledger

-- * Type Conversions

-- | Convert a cardano-api 'ExecutionUnits' into a cardano-ledger 'ExUnits'
toLedgerExUnits :: ExecutionUnits -> Ledger.ExUnits
toLedgerExUnits = toAlonzoExUnits

-- | Convert a cardano-ledger 'ExUnits' into a cardano-api 'ExecutionUnits'
fromLedgerExUnits :: Ledger.ExUnits -> ExecutionUnits
fromLedgerExUnits = fromAlonzoExUnits
