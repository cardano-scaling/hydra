{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

-- | Simple asserting validators that are primarily useful for testing.
module Hydra.Contract.Dummy where

import Hydra.Prelude

import Hydra.Cardano.Api (PlutusScript, pattern PlutusScriptSerialised)
import PlutusLedgerApi.V3 (ScriptContext (..), ScriptInfo (..), serialiseCompiledCode, unsafeFromBuiltinData)
import PlutusTx (compile)
import PlutusTx.Prelude (check)

dummyValidatorScript :: PlutusScript
dummyValidatorScript =
  PlutusScriptSerialised $
    serialiseCompiledCode
      $$( PlutusTx.compile
            [||
            \ctx ->
              check $ case unsafeFromBuiltinData ctx of
                ScriptContext{scriptContextScriptInfo = SpendingScript{}} -> True
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
