{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Validators where

import PlutusTx.Prelude

import Plutus.MerkleTree (member)
import qualified Plutus.MerkleTree as MT
import Plutus.V1.Ledger.Api (ScriptContext, Validator, mkValidatorScript)
import PlutusTx (UnsafeFromData (..))
import qualified PlutusTx as Plutus

-- | Signature of an untyped validator script.
type ValidatorType = BuiltinData -> BuiltinData -> BuiltinData -> ()

-- | Wrap a typed validator to get the basic `ValidatorType` signature which can
-- be passed to `PlutusTx.compile`.
-- REVIEW: There might be better ways to name this than "wrap"
--
-- TODO: Duplicate from Plutus.Extras, can't use because of circular deps.
wrapValidator ::
  (UnsafeFromData datum, UnsafeFromData redeemer) =>
  (datum -> redeemer -> ScriptContext -> Bool) ->
  ValidatorType
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
wrapValidator f d r p =
  check $
    f
      (unsafeFromBuiltinData d)
      (unsafeFromBuiltinData r)
      (unsafeFromBuiltinData p)
{-# INLINEABLE wrapValidator #-}

-- | A validator for measuring cost of MT membership validation.
merkleTreeMemberValidator :: Validator
merkleTreeMemberValidator =
  mkValidatorScript
    $$( Plutus.compile
          [||
          wrapValidator $
            \() (e, root, proof) _ctx ->
              member e root proof
          ||]
      )

-- | A validator for measuring cost of MT construction.
-- data MtBuilderValidator
merkleTreeBuilderValidator :: Validator
merkleTreeBuilderValidator =
  mkValidatorScript
    $$( Plutus.compile
          [||
          wrapValidator $
            \() (utxos, root) _ctx ->
              MT.rootHash (MT.fromList utxos) == root
          ||]
      )
