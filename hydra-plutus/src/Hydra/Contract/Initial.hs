{-# LANGUAGE TemplateHaskell #-}

-- | Datum and redeemer types, as well as helper functions for the commit
-- validator implemented in aiken.
module Hydra.Contract.Initial where

import PlutusLedgerApi.V3 (
  CurrencySymbol,
  Datum (..),
  Redeemer (Redeemer),
  ToData (toBuiltinData),
  TxOutRef,
 )
import PlutusTx qualified

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
