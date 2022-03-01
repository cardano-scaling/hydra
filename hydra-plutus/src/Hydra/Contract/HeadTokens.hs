{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Minting policy for a single head tokens.
module Hydra.Contract.HeadTokens where

import Ledger hiding (Mint)
import PlutusTx.Prelude

import Hydra.Contract.Head (Head)
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import Hydra.Contract.MintAction (MintAction (Burn, Mint))
import Ledger.Typed.Scripts (ValidatorTypes (..), wrapMintingPolicy)
import Plutus.V1.Ledger.Api (fromBuiltinData)
import Plutus.V1.Ledger.Value (getValue)
import qualified PlutusTx
import PlutusTx.AssocMap as Map

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
validate headValidator _ action context =
  let currency = ownCurrencySymbol context
      minted = getValue $ txInfoMint txInfo
      mintValue = case action of
        Mint -> 1
        Burn -> -1
      (checkQuantities, total) = case Map.lookup currency minted of
        Nothing -> (False, 0)
        Just tokenMap ->
          foldr
            (\q (assertion, n) -> (assertion && (q == mintValue), n + 1))
            (True, 0)
            tokenMap
   in case action of
        Mint ->
          traceIfFalse "minted wrong" $
            checkQuantities && total == nParties + 1
        Burn ->
          traceIfFalse "burnt wrong" $
            checkQuantities && total == nParties + 1
 where
  ScriptContext{scriptContextTxInfo = txInfo} = context

  nParties =
    case scriptOutputsAt headValidator txInfo of
      [(dh, _)] ->
        case getDatum <$> findDatum dh txInfo of
          Nothing -> traceError "expected optional commit datum"
          Just da ->
            case fromBuiltinData @(DatumType Head) da of
              Nothing -> traceError "expected commit datum type, got something else"
              Just Head.Initial{Head.parties = parties} -> length parties
              Just _ -> traceError "unexpected State in datum"
      _ -> traceError "expected single head output"

mintingPolicy :: TxOutRef -> MintingPolicy
mintingPolicy txOutRef =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\vHead ref -> wrapMintingPolicy (validate vHead ref)||])
      `PlutusTx.applyCode` PlutusTx.liftCode Head.validatorHash
      `PlutusTx.applyCode` PlutusTx.liftCode txOutRef

validatorScript :: TxOutRef -> Script
validatorScript = getMintingPolicy . mintingPolicy
