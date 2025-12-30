{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}

module Hydra.Contract.CRS where

import Hydra.Prelude hiding (filter, foldMap, isJust, map, (<$>), (==))

import Hydra.Cardano.Api (PlutusScript, pattern PlutusScriptSerialised)
import Hydra.Plutus.Extras (ValidatorType, wrapValidator)
import PlutusLedgerApi.V3 (
  Datum (..),
  ScriptContext (..),
  serialiseCompiledCode,
  toBuiltinData,
 )
import PlutusTx (CompiledCode, compile)
import PlutusTx.Builtins (BuiltinBLS12_381_G1_Element)

type CRSDatum = [BuiltinBLS12_381_G1_Element]

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
