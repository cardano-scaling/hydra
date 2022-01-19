{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Plutus.MerkleTreeValidator where

import PlutusTx.Prelude

import qualified Ledger.Typed.Scripts as Scripts
import Plutus.MerkleTree (Hash, Proof, member)
import qualified Plutus.MerkleTree as MT
import qualified PlutusTx as Plutus

-- | A validator for measuring cost of MT membership validation.
data MerkleTreeValidator

instance Scripts.ValidatorTypes MerkleTreeValidator where
  type DatumType MerkleTreeValidator = ()
  type RedeemerType MerkleTreeValidator = (BuiltinByteString, Hash, Proof)

merkleTreeValidator :: Scripts.TypedValidator MerkleTreeValidator
merkleTreeValidator =
  Scripts.mkTypedValidator @MerkleTreeValidator
    $$( Plutus.compile
          [||
          \() (e, root, proof) _ctx ->
            member e root proof
          ||]
      )
    $$(Plutus.compile [||wrap||])
 where
  wrap = Scripts.wrapValidator @() @(Scripts.RedeemerType MerkleTreeValidator)

-- | A validator for measuring cost of MT construction.
data MtBuilderValidator

instance Scripts.ValidatorTypes MtBuilderValidator where
  type DatumType MtBuilderValidator = ()
  type RedeemerType MtBuilderValidator = ([BuiltinByteString], Hash)

mtBuilderValidator :: Scripts.TypedValidator MtBuilderValidator
mtBuilderValidator =
  Scripts.mkTypedValidator @MtBuilderValidator
    $$( Plutus.compile
          [||
          \() (utxos, root) _ctx ->
            MT.rootHash (MT.fromList utxos) == root
          ||]
      )
    $$(Plutus.compile [||wrap||])
 where
  wrap = Scripts.wrapValidator @() @(Scripts.RedeemerType MtBuilderValidator)
