{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Minting policy for a single head tokens.
module Hydra.Contract.HeadTokens where

import PlutusTx.Prelude

import Ledger (ScriptContext (..), TxInfo (txInfoMint), ownCurrencySymbol)
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
      minted = txInfoMint txInfo
   in valueOf minted currency (TokenName hydraHeadV1) == 1
 where
  ScriptContext{scriptContextTxInfo = txInfo} = context

-- TODO: Do determine parameters/party from datum as soon as v_head is not
-- parameterized anymore (with something like this below)

-- parameters =
--   case scriptOutputsAt headValidator txInfo of
--     [(dh, _)] ->
--       case getDatum <$> findDatum dh txInfo of
--         Nothing -> traceError "expected optional commit datum"
--         Just da ->
--           case fromBuiltinData @(DatumType Head.State) da of
--             Nothing -> traceError "expected commit datum type, got something else"
--             Just Head.Initial{} -> True
--             Just _ -> traceError "unexpected State in datum"
--     _ -> traceError "expected single head output"

mintingPolicy :: TxOutRef -> MintingPolicy
mintingPolicy txOutRef =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrapMintingPolicy . validate||])
      `PlutusTx.applyCode` PlutusTx.liftCode txOutRef

validatorScript :: TxOutRef -> Script
validatorScript = getMintingPolicy . mintingPolicy
