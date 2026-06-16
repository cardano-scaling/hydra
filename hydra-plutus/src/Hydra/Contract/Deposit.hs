{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoPolyKinds #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:target-version=1.1.0 #-}

-- | The validator used to deposit and recover locked funds
module Hydra.Contract.Deposit where

import PlutusTx.Prelude

import Hydra.Contract.Commit (Commit)
import PlutusLedgerApi.V3 (
  CurrencySymbol,
  Datum (Datum),
  POSIXTime,
  Redeemer (Redeemer),
 )
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusTx qualified

data DepositRedeemer
  = -- | Claims already deposited funds.
    Claim
  | -- | Recovers m number of deposited outputs.
    Recover Integer

-- NOTE: These indices are load-bearing across languages: @validators/deposit.ak@
-- redefines this redeemer structurally and matches on @Claim@/@Recover@, so
-- reordering the constructors here silently breaks deposit/recover validation
-- with no compile error on either side. Keep in sync with @deposit.ak@.
PlutusTx.makeIsDataIndexed ''DepositRedeemer [('Claim, 0), ('Recover, 1)]

-- | Deposit datum containing HeadId, deadline and a list of deposits.
type DepositDatum = (CurrencySymbol, POSIXTime, [Commit])

datum :: DepositDatum -> PlutusV3.Datum
datum a = Datum (toBuiltinData a)

redeemer :: DepositRedeemer -> Redeemer
redeemer a = Redeemer (toBuiltinData a)
