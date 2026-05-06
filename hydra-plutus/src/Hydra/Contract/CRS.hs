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
-- Verifies the KZG membership pairing identity:
--
-- > e(G1, commitment) = e(P_S(τ)·G1, proof)
--
-- Argument mapping:
--
-- * @commitment@: A(τ)·G2 — the accumulator commitment from the Closed datum
-- * @proof@: Q(τ)·G2 — the quotient polynomial committed over G2, proving subset membership
-- * @crsG1@: @[G1, τ·G1, ...]@ — used on-chain to compute @P_S(τ)·G1@ via MSM
-- * @ints@: integer encodings of element hashes, defining @P_S(X) = ∏(X − sᵢ)@
--
-- Note: the underlying 'checkMembership' takes @(crs, commitment, scalars, proof)@;
-- this wrapper uses the more natural @(commitment, proof, crs, scalars)@ order.
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
