{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Plutus.Extras (
  module Hydra.Plutus.Extras,
  module Hydra.Plutus.Extras.Time,
) where

import Hydra.Prelude

import Hydra.Plutus.Extras.Time

import Cardano.Api (
  IsPlutusScriptLanguage,
  PlutusScriptVersion,
  SerialiseAsRawBytes (serialiseToRawBytes),
  hashScript,
  pattern PlutusScript,
 )
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V3 (
  Datum (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (..),
  getRedeemer,
  scriptContextRedeemer,
  scriptContextScriptInfo,
 )
import PlutusTx (BuiltinData, UnsafeFromData (..))
import PlutusTx.Prelude (BuiltinUnit, check, toBuiltin)

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
scriptValidatorHash :: IsPlutusScriptLanguage lang => PlutusScriptVersion lang -> SerialisedScript -> ScriptHash
scriptValidatorHash version =
  ScriptHash
    . toBuiltin
    . serialiseToRawBytes
    . hashScript
    . PlutusScript version
    . PlutusScriptSerialised
