{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Util where

import Plutus.V1.Ledger.Value (isZero)
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  TokenName (..),
  TxInfo (TxInfo, txInfoMint),
  TxOut (txOutValue),
  Value (getValue),
 )
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude

hydraHeadV1 :: BuiltinByteString
hydraHeadV1 = "HydraHeadV1"

-- | Checks that the output contains the state token (ST) with the head
-- 'CurrencySymbol' and 'TokenName' of 'hydraHeadV1'
hasST :: CurrencySymbol -> Value -> Bool
hasST headPolicyId v =
  isJust $
    find
      (\(cs, tokenMap) -> cs == headPolicyId && hasHydraToken tokenMap)
      (Map.toList $ getValue v)
 where
  hasHydraToken tm =
    isJust $ find (\(tn, q) -> q == 1 && TokenName hydraHeadV1 == tn) (Map.toList tm)
{-# INLINEABLE hasST #-}

-- | Checks if all the state token (ST) for list of parties containing specific
-- 'CurrencySymbol' are burnt.
mustBurnST :: Value -> CurrencySymbol -> Bool
mustBurnST val headCurrencySymbol =
  case Map.lookup headCurrencySymbol (getValue val) of
    Nothing -> True
    Just tokenMap ->
      case Map.lookup (TokenName hydraHeadV1) tokenMap of
        Nothing -> True
        Just v -> v == negate 1
{-# INLINEABLE mustBurnST #-}

mustNotMintOrBurn :: TxInfo -> Bool
mustNotMintOrBurn TxInfo{txInfoMint} =
  traceIfFalse "minting or burning is forbidden" $
    isZero txInfoMint
{-# INLINEABLE mustNotMintOrBurn #-}

mustPreserveValue :: Value -> Value -> Bool
mustPreserveValue outValue headOutValue =
  traceIfFalse "head value is not preserved" $
    outValue == headOutValue
{-# INLINEABLE mustPreserveValue #-}

headOutputValue :: [TxOut] -> Value
headOutputValue (headOutput : _outputs) = txOutValue headOutput
headOutputValue _ = traceError "does not have at least head output"
{-# INLINEABLE headOutputValue #-}
