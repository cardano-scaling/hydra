{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Hydra.Contract.CRS where

import Hydra.Prelude hiding (filter, foldMap, isJust, map, (<$>), (==))

import Hydra.Cardano.Api (PlutusScript, pattern PlutusScriptSerialised)
import Hydra.Plutus.Extras (ValidatorType, wrapValidator)
import PlutusLedgerApi.V3 (
  BuiltinByteString,
  Datum (..),
  ScriptContext (..),
  serialiseCompiledCode,
  toBuiltinData,
 )
import PlutusTx (CompiledCode, compile)

type CRSDatum = [BuiltinByteString]

datum :: CRSDatum -> Datum
datum a = Datum (toBuiltinData a)

{-# INLINEABLE crsValidator #-}
crsValidator ::
  CRSDatum ->
  () ->
  ScriptContext ->
  Bool
crsValidator _ _ _ = True

crsValidatorScript :: CompiledCode ValidatorType
crsValidatorScript =
  $$( PlutusTx.compile
        [||wrap crsValidator||]
    )
 where
  wrap = wrapValidator @CRSDatum @()

validatorScript :: PlutusScript
validatorScript = PlutusScriptSerialised $ serialiseCompiledCode crsValidatorScript
