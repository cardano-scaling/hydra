{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
-- Plutus core version to compile to. In babbage era, that is Cardano protocol
-- version 7 and 8, only plutus-core version 1.0.0 is available.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | The validator used to deposit and recover locked funds
module Hydra.Contract.Deposit where

import "plutus-tx" PlutusTx.Prelude

import Hydra.Contract.Commit (Commit)
import "plutus-ledger-api" PlutusLedgerApi.V3 (
  CurrencySymbol,
  Datum (Datum),
  POSIXTime,
  Redeemer (Redeemer),
 )
import "plutus-ledger-api" PlutusLedgerApi.V3 qualified as PlutusV3
import "plutus-tx" PlutusTx qualified

data DepositRedeemer
  = -- | Claims already deposited funds.
    Claim CurrencySymbol
  | -- | Recovers m number of deposited outputs.
    Recover Integer

PlutusTx.unstableMakeIsData ''DepositRedeemer

-- | Deposit datum containing HeadId, deadline and a list of deposits.
type DepositDatum = (CurrencySymbol, POSIXTime, [Commit])

datum :: DepositDatum -> PlutusV3.Datum
datum a = Datum (toBuiltinData a)

redeemer :: DepositRedeemer -> Redeemer
redeemer a = Redeemer (toBuiltinData a)
