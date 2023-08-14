{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}
-- Plutus core version to compile to. Cardano protocol version 8 is only
-- supporting plutus-core version 1.0.0.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Validators where

import PlutusTx.Prelude

import Hydra.Plutus.Extras (wrapValidator)
import qualified Plutus.MerkleTree as MT
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V2 (ScriptContext)
import qualified PlutusTx as Plutus

-- | A validator for measuring cost of MT membership validation.
merkleTreeMemberValidator :: SerialisedScript
merkleTreeMemberValidator =
  serialiseCompiledCode
    $$( Plutus.compile
          [||
          wrapValidator $
            \() (e, root, proof) (_ :: ScriptContext) ->
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
            \() (utxos, root) (_ :: ScriptContext) ->
              MT.rootHash (MT.fromList utxos) == root
          ||]
      )
