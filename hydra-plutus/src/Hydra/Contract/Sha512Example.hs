{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

-- | Simple asserting validators that are primarily useful for testing.
module Hydra.Contract.Sha512Example where

import Hydra.Prelude

import Hydra.Cardano.Api (PlutusScript, pattern PlutusScriptSerialised)
import PlutusLedgerApi.V3 (ScriptContext (..), ScriptInfo (..), serialiseCompiledCode, unsafeFromBuiltinData)
import PlutusTx (compile)
import PlutusTx.Builtins (sha3_512)
import PlutusTx.Prelude (Eq (..), check)

trivialCheck :: Bool
trivialCheck =
  let x = sha3_512 "aaaaaaaa"
   in x PlutusTx.Prelude.== x

dummyValidatorScript :: PlutusScript
dummyValidatorScript =
  PlutusScriptSerialised $
    serialiseCompiledCode
      $$( PlutusTx.compile
            [||
            \ctx ->
              check $ case unsafeFromBuiltinData ctx of
                ScriptContext{scriptContextScriptInfo = SpendingScript{}} -> trivialCheck
                _ -> False
            ||]
        )

dummyRewardingScript :: PlutusScript
dummyRewardingScript =
  PlutusScriptSerialised $
    serialiseCompiledCode
      $$( PlutusTx.compile
            [||
            \ctx ->
              check $ case unsafeFromBuiltinData ctx of
                ScriptContext{scriptContextScriptInfo = CertifyingScript{}} -> trivialCheck
                ScriptContext{scriptContextScriptInfo = RewardingScript{}} -> trivialCheck
                _ -> False
            ||]
        )
