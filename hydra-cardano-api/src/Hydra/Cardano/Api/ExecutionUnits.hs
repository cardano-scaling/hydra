module Hydra.Cardano.Api.ExecutionUnits where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.Scripts as Ledger

-- * Type Conversions

-- | Convert a cardano-api's 'ExecutionUnits' into a cardano-ledger's 'ExUnits'
toLedgerExUnits :: ExecutionUnits -> Ledger.ExUnits
toLedgerExUnits = toAlonzoExUnits

-- | Convert a cardano-ledger's 'ExUnits' into a cardano-api's 'ExecutionUnits'
fromLedgerExUnits :: Ledger.ExUnits -> ExecutionUnits
fromLedgerExUnits = fromAlonzoExUnits
