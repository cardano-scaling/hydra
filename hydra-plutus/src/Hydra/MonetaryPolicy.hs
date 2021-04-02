{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.MonetaryPolicy where

import Ledger (CurrencySymbol, MonetaryPolicy, TxId, mkMonetaryPolicyScript, scriptCurrencySymbol)
import qualified Ledger.Contexts as V
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Prelude

validate :: (TxId, Integer) -> V.PolicyCtx -> Bool
validate _ _ = True

curPolicy :: (TxId, Integer) -> MonetaryPolicy
curPolicy cur =
  mkMonetaryPolicyScript $
    $$(PlutusTx.compile [||\c -> Scripts.wrapMonetaryPolicy (validate c)||])
      `PlutusTx.applyCode` PlutusTx.liftCode cur

hydraCurrencySymbol :: (TxId, Integer) -> CurrencySymbol
hydraCurrencySymbol = scriptCurrencySymbol . curPolicy
