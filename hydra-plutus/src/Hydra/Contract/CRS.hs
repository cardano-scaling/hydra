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
import Plutus.Crypto.BlsUtils (getFinalPoly, getG2Commitment, mkScalar)
import PlutusLedgerApi.V3 (
  ScriptContext (..),
  serialiseCompiledCode,
 )
import PlutusTx (CompiledCode, compile)
import PlutusTx.Builtins (
  BuiltinBLS12_381_G1_Element,
  BuiltinBLS12_381_G2_Element,
  bls12_381_finalVerify,
  bls12_381_millerLoop,
 )

type CRSDatum = [BuiltinBLS12_381_G2_Element]

-- | Core BLS pairing check shared by full fanout and partial fanout.
--
-- Verifies the KZG membership pairing identity:
--
-- > e(commitment, G2) = e(proof, P_S(τ)·G2)
--
-- Argument mapping:
--
-- * @commitment@: A(τ)·G1 — the accumulator commitment from the Closed datum
-- * @proof@: Q(τ)·G1 — the quotient polynomial committed over G1, proving subset membership
-- * @crsG2@: @[G2, τ·G2, ...]@ — used on-chain to compute @P_S(τ)·G2@ via MSM
-- * @ints@: integer encodings of element hashes, defining @P_S(X) = ∏(X − sᵢ)@
{-# INLINEABLE checkMembershipPairing #-}
checkMembershipPairing ::
  BuiltinBLS12_381_G1_Element ->
  BuiltinBLS12_381_G1_Element ->
  CRSDatum ->
  [Integer] ->
  Bool
checkMembershipPairing commitment proof crsG2 ints =
  case crsG2 of
    [] -> False
    (g2 : _) ->
      bls12_381_finalVerify
        (bls12_381_millerLoop commitment g2)
        (bls12_381_millerLoop proof (getG2Commitment crsG2 (getFinalPoly (fmap mkScalar ints))))

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
