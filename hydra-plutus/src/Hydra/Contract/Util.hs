{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Util where

import Plutus.V1.Ledger.Value (isZero)
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  TokenName (..),
  TxInfo (TxInfo, txInfoMint),
  Value (getValue),
  toBuiltinData,
 )
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Builtins (serialiseData)
import PlutusTx.Prelude

hydraHeadV1 :: BuiltinByteString
hydraHeadV1 = "HydraHeadV1"

-- | Checks that the output contains the state token (ST) with the head
-- 'CurrencySymbol' and 'TokenName' of 'hydraHeadV1'
hasST :: CurrencySymbol -> Value -> Bool
hasST headPolicyId v =
  maybe False id $ do
    tokenMap <- Map.lookup headPolicyId $ getValue v
    quantity <- Map.lookup (TokenName hydraHeadV1) tokenMap
    pure $ quantity == 1
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

infix 4 ===

-- | Checks for exact exuality between two serialized values
-- Equality on value is very memory intensive as it's defined on associative
-- lists and Map equality is implemented. Instead we can be more strict and
-- require EXACTLY the same value and compare using the serialised bytes.
(===) :: Value -> Value -> Bool
(===) val val' =
  serialiseData (toBuiltinData val) == serialiseData (toBuiltinData val')
{-# INLINEABLE (===) #-}
