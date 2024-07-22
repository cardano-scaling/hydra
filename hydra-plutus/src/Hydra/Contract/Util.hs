{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Util where

import Hydra.Contract.Error (ToErrorCode (..))
import Hydra.Contract.HeadError (HeadError (..), errorCode)
import Hydra.Data.Party (Party)
import Hydra.Prelude (Show)
import PlutusLedgerApi.V1.Value (isZero)
import PlutusLedgerApi.V3 (
  CurrencySymbol,
  TokenName (..),
  TxInfo (TxInfo, txInfoMint),
  Value (getValue),
  toBuiltinData,
 )
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins (serialiseData)
import PlutusTx.Prelude

hydraHeadV1 :: BuiltinByteString
hydraHeadV1 = "HydraHeadV1"

-- | Checks that the output contains the state token (ST) with the head
-- 'CurrencySymbol' and 'TokenName' of 'hydraHeadV1'
hasST :: CurrencySymbol -> Value -> Bool
hasST headPolicyId v =
  fromMaybe False $ do
    tokenMap <- AssocMap.lookup headPolicyId $ getValue v
    quantity <- AssocMap.lookup (TokenName hydraHeadV1) tokenMap
    pure $ quantity == 1
{-# INLINEABLE hasST #-}

-- | Checks all tokens related to some specific `CurrencySymbol`.
--
-- This checks both PTs and ST are burnt.
mustBurnAllHeadTokens :: Value -> CurrencySymbol -> [Party] -> Bool
mustBurnAllHeadTokens minted headCurrencySymbol parties =
  traceIfFalse $(errorCode BurntTokenNumberMismatch) $
    burntTokens == length parties + 1
 where
  burntTokens =
    case AssocMap.lookup headCurrencySymbol (getValue minted) of
      Nothing -> 0
      Just tokenMap -> negate $ sum tokenMap
{-# INLINEABLE mustBurnAllHeadTokens #-}

-- | Checks if the state token (ST) for list of parties containing specific
-- 'CurrencySymbol' are burnt.
mustBurnST :: Value -> CurrencySymbol -> Bool
mustBurnST val headCurrencySymbol =
  case AssocMap.lookup headCurrencySymbol (getValue val) of
    Nothing -> False
    Just tokenMap ->
      case AssocMap.lookup (TokenName hydraHeadV1) tokenMap of
        Nothing -> False
        Just v -> v == negate 1
{-# INLINEABLE mustBurnST #-}

mustNotMintOrBurn :: TxInfo -> Bool
mustNotMintOrBurn TxInfo{txInfoMint} =
  traceIfFalse "U01" $
    isZero txInfoMint
{-# INLINEABLE mustNotMintOrBurn #-}

infix 4 ===

-- | Checks for exact equality between two serialized values.
-- Equality on value is very memory intensive as it's defined on associative
-- lists and `AssocMap` equality is implemented. Instead we can be more strict and
-- require EXACTLY the same value and compare using the serialised bytes.
(===) :: Value -> Value -> Bool
(===) val val' =
  serialiseData (toBuiltinData val) == serialiseData (toBuiltinData val')
{-# INLINEABLE (===) #-}

-- * Errors

data UtilError
  = MintingOrBurningIsForbidden
  deriving stock (Show)

instance ToErrorCode UtilError where
  toErrorCode = \case
    MintingOrBurningIsForbidden -> "U01"
