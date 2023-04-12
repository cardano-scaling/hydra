{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Validators where

import PlutusTx.Prelude

import Plutus.MerkleTree (member)
import qualified Plutus.MerkleTree as MT
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import qualified PlutusTx as Plutus
import Test.Plutus.Validator (wrapValidator)

-- | A validator for measuring cost of MT membership validation.
merkleTreeMemberValidator :: SerialisedScript
merkleTreeMemberValidator =
  serialiseCompiledCode
    $$( Plutus.compile
          [||
          wrapValidator $
            \() (e, root, proof) _ctx ->
              member e root proof
          ||]
      )

-- | A validator for measuring cost of MT construction.
-- data MtBuilderValidator
merkleTreeBuilderValidator :: SerialisedScript
merkleTreeBuilderValidator =
  serialiseCompiledCode
    $$( Plutus.compile
          [||
          wrapValidator $
            \() (utxos, root) _ctx ->
              MT.rootHash (MT.fromList utxos) == root
          ||]
      )
