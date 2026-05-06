{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}

module Hydra.Contract.CRS (
  CRSDatum,
  checkMembershipPairing,
  crsValidatorScript,
  validatorScript,
) where

import Hydra.Prelude hiding (filter, foldMap, isJust, map, (<$>), (==))

import Hydra.Cardano.Api (PlutusScript, pattern PlutusScriptSerialised)
import Hydra.Plutus.Extras (ValidatorType, wrapValidator)
import Plutus.Crypto.Accumulator (checkMembership)
import Plutus.Crypto.BlsUtils (mkScalar)
import PlutusLedgerApi.V3 (
  ScriptContext (..),
  serialiseCompiledCode,
 )
import PlutusTx (CompiledCode, compile)
import PlutusTx.Builtins (
  BuiltinBLS12_381_G1_Element,
  BuiltinBLS12_381_G2_Element,
 )

type CRSDatum = [BuiltinBLS12_381_G1_Element]

-- | Core BLS pairing check shared by full fanout and partial fanout.
--
-- Verifies: @e(G1, commitment) = e(S(τ)·G1, proof)@
-- where S is the subset polynomial derived from @scalars@.
-- Delegates to 'Plutus.Crypto.Accumulator.checkMembership'.
{-# INLINEABLE checkMembershipPairing #-}
checkMembershipPairing ::
  BuiltinBLS12_381_G2_Element ->
  BuiltinBLS12_381_G2_Element ->
  CRSDatum ->
  [Integer] ->
  Bool
checkMembershipPairing commitment proof crsG1 ints =
  checkMembership crsG1 commitment (fmap mkScalar ints) proof

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
