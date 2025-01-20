{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Hydra.Contract.Dummy where

import Hydra.Prelude

import Hydra.Cardano.Api (pattern PlutusScriptSerialised)
import Hydra.Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import PlutusLedgerApi.V3 (BuiltinData, ScriptContext, ScriptHash, SerialisedScript, serialiseCompiledCode, toOpaque)
import PlutusTx (CompiledCode, compile)

dummyValidator :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
dummyValidator _ _ _ = True

compiledDummyValidator :: CompiledCode ValidatorType
compiledDummyValidator =
  $$(PlutusTx.compile [||fakeWrap dummyValidator||])
 where
  wrap = wrapValidator @BuiltinData @BuiltinData

fakeWrap ::
  (datum -> redeemer -> ScriptContext -> Bool) ->
  ValidatorType
fakeWrap _ _ = toOpaque ()

dummyValidatorScript :: SerialisedScript
dummyValidatorScript = serialiseCompiledCode compiledDummyValidator

dummyValidatorHash :: ScriptHash
dummyValidatorHash = scriptValidatorHash $ PlutusScriptSerialised dummyValidatorScript
