{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Hydra.Contract.CRS where

import Hydra.Prelude hiding (foldMap, (<$>), (==), filter, map, isJust)

import Hydra.Cardano.Api (PlutusScript, pattern PlutusScriptSerialised)
import Hydra.Plutus.Extras (wrapValidator, ValidatorType)
import PlutusLedgerApi.V3 (
  ScriptContext (..),
  serialiseCompiledCode,
 )
import PlutusTx (compile, unstableMakeIsData, CompiledCode)
import PlutusTx.Prelude (BuiltinBLS12_381_G1_Element)

newtype CRSDatum = CRSDatum
  { crs :: [BuiltinBLS12_381_G1_Element]
  }
  deriving stock (Show, Generic)

unstableMakeIsData ''CRSDatum

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

