{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Minting policy for a single head tokens.
module Hydra.Contract.HeadTokens where

import PlutusTx.Prelude

import qualified Hydra.Contract.Head as Head
import Hydra.Contract.MintAction (MintAction (Burn, Mint))
import Ledger (ScriptContext (..), TxInfo (txInfoMint), ValidatorHash, ownCurrencySymbol)
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
validate ::
  -- | Head validator
  ValidatorHash ->
  TxOutRef ->
  MintAction ->
  ScriptContext ->
  Bool
validate _headValidator _ action context =
  let currency = ownCurrencySymbol context
      minted = txInfoMint txInfo
      quantityST = valueOf minted currency (TokenName hydraHeadV1)
   in case action of
        Mint -> traceIfFalse "minted not 1" $ quantityST == 1
        Burn -> traceIfFalse "burnt not 1" $ quantityST == -1
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
    $$(PlutusTx.compile [||\vHead ref -> wrapMintingPolicy (validate vHead ref)||])
      `PlutusTx.applyCode` PlutusTx.liftCode Head.validatorHash
      `PlutusTx.applyCode` PlutusTx.liftCode txOutRef

validatorScript :: TxOutRef -> Script
validatorScript = getMintingPolicy . mintingPolicy
