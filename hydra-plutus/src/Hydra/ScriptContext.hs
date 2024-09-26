{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

-- | A custom ScriptContext and TxInfo which only "decodes" the fields we need.
module Hydra.ScriptContext where

import PlutusLedgerApi.V2.Contexts hiding (
  ScriptContext,
  TxInfo (..),
  scriptContextPurpose,
  scriptContextTxInfo,
 )
import PlutusTx.Prelude

import Plutus.Script.Utils.Typed (IsScriptContext (..))
import PlutusLedgerApi.V2 (
  Address (..),
  Credential (..),
  CurrencySymbol,
  Datum,
  DatumHash,
  Map,
  OutputDatum,
  PubKeyHash,
  ScriptHash,
  Value,
 )
import PlutusTx (makeIsDataIndexed)
import PlutusTx.AssocMap (lookup)

-- * Tx info

data TxInfo = TxInfo
  { txInfoInputs :: [TxInInfo]
  -- ^ Transaction inputs; cannot be an empty list
  , txInfoReferenceInputs :: BuiltinData
  -- ^ Transaction reference inputs
  , txInfoOutputs :: [TxOut]
  -- ^ Transaction outputs
  , txInfoFee :: Value
  -- ^ The fee paid by this transaction.
  , txInfoMint :: Value
  -- ^ The 'Value' minted by this transaction.
  , txInfoDCert :: BuiltinData
  -- ^ Digests of certificates included in this transaction
  , txInfoWdrl :: BuiltinData
  -- ^ Withdrawals
  , -- XXX: using POSIXTimeRange adds ~300 bytes, needed for Head
    txInfoValidRange :: BuiltinData
  -- ^ The valid range for the transaction.
  , txInfoSignatories :: [PubKeyHash]
  -- ^ Signatures provided with the transaction, attested that they all signed the tx
  , txInfoRedeemers :: BuiltinData
  -- ^ A table of redeemers attached to the transaction
  , txInfoData :: Map DatumHash Datum
  -- ^ The lookup table of datums attached to the transaction
  , txInfoId :: BuiltinData
  -- ^ Hash of the pending transaction body (i.e. transaction excluding witnesses)
  }

makeIsDataIndexed ''TxInfo [('TxInfo, 0)]

-- * Script context

-- | The context that the currently-executing script can access.
data ScriptContext = ScriptContext
  { scriptContextTxInfo :: TxInfo
  -- ^ information about the transaction the currently-executing script is included in
  , scriptContextPurpose :: ScriptPurpose
  -- ^ the purpose of the currently-executing script
  }

makeIsDataIndexed ''ScriptContext [('ScriptContext, 0)]

instance IsScriptContext ScriptContext

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
findOwnInput ScriptContext{scriptContextTxInfo = TxInfo{txInfoInputs}, scriptContextPurpose = Spending txOutRef} =
  find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs
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
ownCurrencySymbol ScriptContext{scriptContextPurpose = Minting cs} = cs
ownCurrencySymbol _ = traceError "Lh" -- "Can't get currency symbol of the current validator script"
{-# INLINEABLE ownCurrencySymbol #-}
