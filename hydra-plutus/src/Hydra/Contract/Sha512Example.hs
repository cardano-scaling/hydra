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

dummyValidatorScript :: PlutusScript
dummyValidatorScript =
  PlutusScriptSerialised $
    serialiseCompiledCode
      $$( PlutusTx.compile
            [||
            \ctx ->
              check $ case unsafeFromBuiltinData ctx of
                ScriptContext{scriptContextScriptInfo = SpendingScript{}} -> "" PlutusTx.Prelude.== sha3_512 ""
                _ -> False
            ||]
        )

dummyMintingScript :: PlutusScript
dummyMintingScript =
  PlutusScriptSerialised $
    serialiseCompiledCode
      $$( PlutusTx.compile
            [||
            \ctx ->
              check $ case unsafeFromBuiltinData ctx of
                ScriptContext{scriptContextScriptInfo = MintingScript{}} -> True
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
                ScriptContext{scriptContextScriptInfo = CertifyingScript{}} -> True
                ScriptContext{scriptContextScriptInfo = RewardingScript{}} -> True
                _ -> False
            ||]
        )
