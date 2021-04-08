{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.MonetaryPolicy where

import Ledger (CurrencySymbol, MonetaryPolicy, mkMonetaryPolicyScript, scriptCurrencySymbol)
import qualified Ledger.Contexts as V
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Prelude

type CurrencyId = Integer

validate :: CurrencyId -> V.PolicyCtx -> Bool
validate _ _ = True

curPolicy :: CurrencyId -> MonetaryPolicy
curPolicy cur =
  mkMonetaryPolicyScript $
    $$(PlutusTx.compile [||\c -> Scripts.wrapMonetaryPolicy (validate c)||])
      `PlutusTx.applyCode` PlutusTx.liftCode cur

hydraCurrencySymbol :: CurrencyId -> CurrencySymbol
hydraCurrencySymbol = scriptCurrencySymbol . curPolicy
