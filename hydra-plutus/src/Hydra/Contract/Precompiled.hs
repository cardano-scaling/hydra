{-# LANGUAGE TemplateHaskell #-}

-- | Scripts exported by the native build, used when the Plutus plugin cannot
-- run in the cross-compiler. Keep PIR so runtime parameters can still be applied.
module Hydra.Contract.Precompiled where

import Hydra.Prelude

import Data.FileEmbed (embedFile)
import Hydra.Plutus.Extras (MintingPolicyType, ValidatorType)
import PlutusLedgerApi.V3 (TxOutRef)
import PlutusTx.Builtins (BuiltinByteString)
import PlutusTx.Code (CompiledCode, CompiledCodeIn (SerializedCode))

unappliedValidator :: CompiledCode (BuiltinByteString -> ValidatorType)
unappliedValidator = SerializedCode $(embedFile "cross-scripts/head.flat") (Just $(embedFile "cross-scripts/head.flat.pir")) mempty

unappliedMintingPolicy :: CompiledCode (TxOutRef -> MintingPolicyType)
unappliedMintingPolicy = SerializedCode $(embedFile "cross-scripts/mint.flat") (Just $(embedFile "cross-scripts/mint.flat.pir")) mempty

crsValidatorScript :: CompiledCode ValidatorType
crsValidatorScript = SerializedCode $(embedFile "cross-scripts/crs.flat") (Just $(embedFile "cross-scripts/crs.flat.pir")) mempty
