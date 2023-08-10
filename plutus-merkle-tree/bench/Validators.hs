{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}
-- Plutus core version to compile to. Cardano protocol version 8 is only
-- supporting plutus-core version 1.0.0.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Validators where

import PlutusTx.Prelude

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
              MT.member e root proof
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
