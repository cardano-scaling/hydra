{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.MonetaryPolicy where

import Ledger (
  MonetaryPolicy,
  MonetaryPolicyHash,
  mkMonetaryPolicyScript,
  monetaryPolicyHash,
 )
import qualified Ledger.Contexts as V
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Prelude

type CurrencyId = Integer -- FIXME: This should ultimately be a TxOutRef

validate :: CurrencyId -> V.PolicyCtx -> Bool
validate _ _ = True

{- ORMOLU_DISABLE -}
hydraPolicy :: CurrencyId -> MonetaryPolicy
hydraPolicy cur =
  mkMonetaryPolicyScript $
    $$(PlutusTx.compile [||\c -> Scripts.wrapMonetaryPolicy (validate c)||])
      `PlutusTx.applyCode` PlutusTx.liftCode cur
{- ORMOLU_ENABLE -}

hydraPolicyHash :: CurrencyId -> MonetaryPolicyHash
hydraPolicyHash = monetaryPolicyHash . hydraPolicy
