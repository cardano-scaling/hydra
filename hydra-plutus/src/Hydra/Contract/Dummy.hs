{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
-- Plutus core version to compile to. In babbage era, that is Cardano protocol
-- version 7 and 8, only plutus-core version 1.0.0 is available.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Hydra.Contract.Dummy where

import Hydra.Cardano.Api (PlutusScriptVersion (PlutusScriptV3))
import Hydra.Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import Hydra.Prelude

import PlutusLedgerApi.V3 (ScriptContext (scriptContextScriptInfo), ScriptHash, ScriptInfo (RewardingScript), SerialisedScript, UnsafeFromData (unsafeFromBuiltinData), getRedeemer, scriptContextRedeemer, serialiseCompiledCode, toBuiltinData)
import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude (check)

type D = ()
type R = ()

dummyValidator :: D -> R -> ScriptContext -> Bool
dummyValidator _ _ _ = True

compiledDummyValidator :: CompiledCode ValidatorType
compiledDummyValidator =
  $$(PlutusTx.compile [||wrap dummyValidator||])
 where
  wrap = wrapValidator @D @R

wrapFakeStakeValidator ::
  (UnsafeFromData datum, UnsafeFromData redeemer) =>
  (datum -> redeemer -> ScriptContext -> Bool) ->
  ValidatorType
wrapFakeStakeValidator f c =
  let
    context = unsafeFromBuiltinData c
   in
    check $ case scriptContextScriptInfo context of
      RewardingScript _cred ->
        let datum = unsafeFromBuiltinData $ toBuiltinData ()
            redeemer = unsafeFromBuiltinData $ getRedeemer $ scriptContextRedeemer context
         in f datum redeemer context
      _ -> False
{-# INLINEABLE wrapFakeStakeValidator #-}

compiledDummyStakeValidator :: CompiledCode ValidatorType
compiledDummyStakeValidator =
  $$(PlutusTx.compile [||wrapFakeStakeValidator dummyValidator||])
 where
  wrap = wrapValidator @D @R

dummyValidatorScript :: SerialisedScript
dummyValidatorScript = serialiseCompiledCode compiledDummyValidator

dummyStakeValidatorScript :: SerialisedScript
dummyStakeValidatorScript = serialiseCompiledCode compiledDummyStakeValidator

dummyValidatorHash :: ScriptHash
dummyValidatorHash = scriptValidatorHash PlutusScriptV3 dummyValidatorScript
