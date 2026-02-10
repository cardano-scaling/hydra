{-# LANGUAGE TemplateHaskell #-}

-- | Datum and redeemer types, as well as helper functions for the commit
-- validator implemented in aiken.
module Hydra.Contract.Initial where

import "plutus-ledger-api" PlutusLedgerApi.V3 (
  CurrencySymbol,
  Datum (..),
  Redeemer (Redeemer),
  ToData (toBuiltinData),
  TxOutRef,
 )
import "plutus-tx" PlutusTx qualified

type DatumType = CurrencySymbol

type RedeemerType = InitialRedeemer

data InitialRedeemer
  = ViaAbort
  | ViaCommit
      { committedRefs :: [TxOutRef]
      -- ^ Points to the committed Utxo.
      }

PlutusTx.unstableMakeIsData ''InitialRedeemer

datum :: DatumType -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: RedeemerType -> Redeemer
redeemer a = Redeemer (toBuiltinData a)
