{-# LANGUAGE TemplateHaskell #-}

-- | Minting policy for a single head tokens.
module Hydra.Contract.HeadTokens where

import PlutusTx.Prelude

import Ledger (
  ScriptContext (
    ScriptContext,
    scriptContextTxInfo
  ),
  TxInfo (txInfoMint),
  ownCurrencySymbol,
 )
import Ledger.Typed.Scripts (wrapMintingPolicy)
import Plutus.V1.Ledger.Api (
  MintingPolicy,
  Script,
  TokenName (TokenName),
  TxOutRef,
  getMintingPolicy,
  mkMintingPolicyScript,
 )
import Plutus.V1.Ledger.Value (valueOf)
import qualified PlutusTx

hydraHeadV1 :: BuiltinByteString
hydraHeadV1 = "HydraHeadV1"

{-# INLINEABLE validate #-}
validate :: TxOutRef -> () -> ScriptContext -> Bool
validate _ _ context =
  let currency = ownCurrencySymbol context
      ScriptContext{scriptContextTxInfo = txInfo} = context
      minted = txInfoMint txInfo
   in valueOf minted currency (TokenName hydraHeadV1) == 1

mintingPolicy :: TxOutRef -> MintingPolicy
mintingPolicy txOutRef =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrapMintingPolicy . validate||])
      `PlutusTx.applyCode` PlutusTx.liftCode txOutRef

validatorScript :: TxOutRef -> Script
validatorScript = getMintingPolicy . mintingPolicy
