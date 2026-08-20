{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:optimize #-}

module Hydra.Contract.CRS (
  CRSDatum,
  checkMembershipPairing,
  crsValidatorScript,
  validatorScript,
) where

import Hydra.Prelude hiding (filter, foldMap, isJust, map, (<$>), (==), (>=))

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
import PlutusTx.List qualified as L
import PlutusTx.Prelude ((>=))

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
--
-- A subset of N elements yields @P_S@ of degree N, so evaluating @P_S(τ)·G2@
-- consumes N+1 CRS points. 'getG2Commitment' pairs the coefficients with the
-- CRS using 'zipWith', which silently drops the coefficients the CRS cannot
-- cover: an oversized subset would then be checked against a truncated,
-- lower-degree polynomial instead of being rejected. Verifying a different
-- identity than the one asked for is never the safe answer, so bail out
-- whenever the polynomial outruns the CRS.
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
    (g2 : _)
      | L.length ints >= L.length crsG2 -> False
      | otherwise ->
          bls12_381_finalVerify
            (bls12_381_millerLoop commitment g2)
            (bls12_381_millerLoop proof (getG2Commitment crsG2 (getFinalPoly (fmap mkScalar ints))))

-- | Validator for the CRS reference script UTxO.
--
-- This UTxO is published at an __AlwaysFails__ script address, meaning the
-- on-chain rules already make it unspendable — no transaction can ever
-- consume it regardless of what this validator returns.  Returning 'False'
-- here is therefore not strictly required, but it makes the intent
-- self-documenting: the CRS UTxO is a permanently locked datum carrier and
-- must never be spent.
{-# INLINEABLE crsValidator #-}
crsValidator ::
  CRSDatum ->
  () ->
  ScriptContext ->
  Bool
crsValidator _ _ _ = False

crsValidatorScript :: CompiledCode ValidatorType
crsValidatorScript =
  $$( PlutusTx.compile
        [||wrap crsValidator||]
    )
 where
  wrap = wrapValidator @CRSDatum @()

validatorScript :: PlutusScript
validatorScript = PlutusScriptSerialised $ serialiseCompiledCode crsValidatorScript
