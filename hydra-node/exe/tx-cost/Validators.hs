{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Validators where

import PlutusTx.Prelude

import Plutus.Extras (wrapValidator)
import Plutus.MerkleTree (member)
import qualified Plutus.MerkleTree as MT
import Plutus.V1.Ledger.Api (Validator, mkValidatorScript)
import qualified PlutusTx as Plutus

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
