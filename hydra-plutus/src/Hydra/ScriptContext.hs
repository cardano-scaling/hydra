{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

-- | A custom ScriptContext and TxInfo which only "decodes" the fields we need.
module Hydra.ScriptContext where

import PlutusLedgerApi.V3.Contexts hiding (
  ScriptContext,
  TxInfo (..),
  scriptContextScriptInfo,
  scriptContextTxInfo,
 )
import PlutusTx.Prelude
import Hydra.Plutus.Extras (ScriptContext (..), TxInfo(..))

import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  CurrencySymbol,
  Datum,
  DatumHash,
  OutputDatum,
  ScriptHash,
  TxOut (..),
  TxOutRef,
  Value,
 )
import PlutusTx.AssocMap (lookup)

-- * Utilities

-- | Get the list of 'TxOut' outputs of the pending transaction at
-- a given script address.
scriptOutputsAt :: ScriptHash -> TxInfo -> [(OutputDatum, Value)]
scriptOutputsAt h p =
  let flt TxOut{txOutDatum = d, txOutAddress = Address (ScriptCredential s) _, txOutValue} | s == h = Just (d, txOutValue)
      flt _ = Nothing
   in mapMaybe flt (txInfoOutputs p)
{-# INLINEABLE scriptOutputsAt #-}

-- | Get the total value locked by the given validator in this transaction.
valueLockedBy :: TxInfo -> ScriptHash -> Value
valueLockedBy ptx h =
  let outputs = map snd (scriptOutputsAt h ptx)
   in mconcat outputs
{-# INLINEABLE valueLockedBy #-}

-- | Find the input currently being validated.
findOwnInput :: ScriptContext -> Maybe TxInInfo
findOwnInput
  ScriptContext
    { scriptContextTxInfo = TxInfo{txInfoInputs}
    , scriptContextScriptInfo = SpendingScript txOutRef _
    } =
    find
      (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef)
      txInfoInputs
findOwnInput _ = Nothing
{-# INLINEABLE findOwnInput #-}

-- | Find the data corresponding to a data hash, if there is one
findDatum :: DatumHash -> TxInfo -> Maybe Datum
findDatum dsh TxInfo{txInfoData} = lookup dsh txInfoData
{-# INLINEABLE findDatum #-}

-- | Given a UTXO reference and a transaction (`TxInfo`), resolve it to one of the transaction's inputs (`TxInInfo`).
findTxInByTxOutRef :: TxOutRef -> TxInfo -> Maybe TxInInfo
findTxInByTxOutRef outRef TxInfo{txInfoInputs} =
  find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == outRef) txInfoInputs
{-# INLINEABLE findTxInByTxOutRef #-}

-- | The 'CurrencySymbol' of the current validator script.
ownCurrencySymbol :: ScriptContext -> CurrencySymbol
ownCurrencySymbol ScriptContext{scriptContextScriptInfo = MintingScript cs} = cs
ownCurrencySymbol _ =
  -- "Can't get currency symbol of the current validator script"
  traceError "Lh"
{-# INLINEABLE ownCurrencySymbol #-}
