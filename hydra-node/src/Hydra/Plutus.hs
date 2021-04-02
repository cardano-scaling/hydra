-- | Provides an "anticorruption layer" between Hydra types and Plutus types.
--
--While it might be tempting to adopt a /Conformist/ approach and simply reuse types and
--functions from Plutus directly, it seems a better strategy to have a clear separation
--in order to prevent too much adherence inside Hydra to Plutus' concepts and types.
module Hydra.Plutus where

import Hydra.OnChainTransaction.Types
import qualified Ledger as Plutus
import qualified Ledger.Value as Value

toCurrencySymbol :: MonetaryPolicyId -> Plutus.CurrencySymbol
toCurrencySymbol (MonetaryPolicyId hash) = Value.currencySymbol hash

fromCurrencySymbol :: Plutus.CurrencySymbol -> MonetaryPolicyId
fromCurrencySymbol = MonetaryPolicyId . Value.unCurrencySymbol
