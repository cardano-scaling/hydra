{-# LANGUAGE TemplateHaskell #-}

-- | Minting policy for a single head tokens.
module Hydra.Contract.HeadTokens where

import PlutusTx.Prelude

import Ledger.Typed.Scripts (wrapMintingPolicy)
import Plutus.V1.Ledger.Api (
  MintingPolicy,
  Script,
  ScriptContext,
  TxOutRef,
  getMintingPolicy,
  mkMintingPolicyScript,
 )
import qualified PlutusTx

validate :: TxOutRef -> () -> ScriptContext -> Bool
validate _ _ _ = True

mintingPolicy :: TxOutRef -> MintingPolicy
mintingPolicy txOutRef =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrapMintingPolicy . validate||])
      `PlutusTx.applyCode` PlutusTx.liftCode txOutRef

validatorScript :: TxOutRef -> Script
validatorScript = getMintingPolicy . mintingPolicy
