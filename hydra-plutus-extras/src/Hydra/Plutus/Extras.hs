{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Plutus.Extras (
  module Hydra.Plutus.Extras,
  module Hydra.Plutus.Extras.Time,
) where

import Hydra.Prelude hiding (Map)

import Hydra.Plutus.Extras.Time

import Cardano.Api (
  PlutusScriptVersion,
  SerialiseAsRawBytes (serialiseToRawBytes),
  hashScript,
  pattern PlutusScript,
 )
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V1.Value (isZero)
import PlutusLedgerApi.V2 (POSIXTimeRange)
import PlutusLedgerApi.V3 (Datum (..), DatumHash, Interval, Map, PubKeyHash, Redeemer, ScriptHash (..), ScriptInfo (..), TxInInfo, TxOut (..), Value, getRedeemer)
import PlutusTx (
  BuiltinData,
  UnsafeFromData (..),
  makeIsDataIndexed,
 )
import PlutusTx.Prelude (BuiltinUnit, check, toBuiltin, traceIfFalse)

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
    txInfoValidRange :: Interval POSIXTimeRange
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
  , scriptContextRedeemer :: Redeemer
  -- ^ Redeemer for the currently-executing script
  , scriptContextScriptInfo :: ScriptInfo
  -- ^ the purpose of the currently-executing script, along with information associated
  -- with the purpose
  }

makeIsDataIndexed ''ScriptContext [('ScriptContext, 0)]

-- * Vendored from plutus-ledger

-- | Signature of an untyped validator script.
type ValidatorType = BuiltinData -> BuiltinUnit

-- | Wrap a typed validator to get the basic `ValidatorType` signature which can
-- be passed to `PlutusTx.compile`.
-- REVIEW: There might be better ways to name this than "wrap"
wrapValidator ::
  (UnsafeFromData datum, UnsafeFromData redeemer) =>
  (datum -> redeemer -> ScriptContext -> Bool) ->
  ValidatorType
wrapValidator f c =
  let
    context = unsafeFromBuiltinData c
   in
    check $ case scriptContextScriptInfo context of
      SpendingScript _ (Just d) ->
        let datum = unsafeFromBuiltinData $ getDatum d
            redeemer = unsafeFromBuiltinData $ getRedeemer $ scriptContextRedeemer context
         in f datum redeemer context
      _ -> False
{-# INLINEABLE wrapValidator #-}

-- | Signature of an untyped minting policy script.
type MintingPolicyType = BuiltinData -> BuiltinUnit

-- | Wrap a typed minting policy to get the basic `MintingPolicyType` signature
-- which can be passed to `PlutusTx.compile`.
wrapMintingPolicy ::
  UnsafeFromData redeemer =>
  (redeemer -> ScriptContext -> Bool) ->
  MintingPolicyType
wrapMintingPolicy f c =
  let
    context = unsafeFromBuiltinData c
   in
    check $ case scriptContextScriptInfo context of
      MintingScript _ ->
        let redeemer = unsafeFromBuiltinData $ getRedeemer $ scriptContextRedeemer context
         in f redeemer context
      _ -> False
{-# INLINEABLE wrapMintingPolicy #-}

-- * Similar utilities as plutus-ledger

-- | Compute the on-chain 'ScriptHash' for a given serialised plutus script. Use
-- this to refer to another validator script.
scriptValidatorHash :: PlutusScriptVersion lang -> SerialisedScript -> ScriptHash
scriptValidatorHash version =
  ScriptHash
    . toBuiltin
    . serialiseToRawBytes
    . hashScript
    . PlutusScript version
    . PlutusScriptSerialised

mustNotMintOrBurn :: TxInfo -> Bool
mustNotMintOrBurn TxInfo{txInfoMint} =
  traceIfFalse "U01" $
    isZero txInfoMint
{-# INLINEABLE mustNotMintOrBurn #-}
