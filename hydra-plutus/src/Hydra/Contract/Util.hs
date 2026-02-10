{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Util where

import "hydra-prelude" Hydra.Prelude (Show)
import "plutus-ledger-api" PlutusLedgerApi.V1.Value (isZero)
import "plutus-ledger-api" PlutusLedgerApi.V3 (

import Hydra.Contract.Commit
import Hydra.Contract.Error (ToErrorCode (..))
import Hydra.Contract.HeadError (HeadError (..), errorCode)
import Hydra.Data.Party (Party)
  Address (..),
  Credential (..),
  CurrencySymbol,
  MintValue,
  OutputDatum (..),
  ScriptHash (..),
  TokenName (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
  Value (getValue),
  mintValueBurned,
  mintValueMinted,
  mintValueToMap,
 )
import "plutus-tx" PlutusTx.AssocMap qualified as AssocMap
import "plutus-tx" PlutusTx.Builtins qualified as Builtins
import "plutus-tx" PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import "plutus-tx" PlutusTx.Foldable qualified as F
import "plutus-tx" PlutusTx.List qualified as L
import "plutus-tx" PlutusTx.Prelude

hydraHeadV1 :: BuiltinByteString
hydraHeadV1 = stringToBuiltinByteString "HydraHeadV1"
{-# INLINEABLE hydraHeadV1 #-}

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
mustBurnAllHeadTokens :: MintValue -> CurrencySymbol -> [Party] -> Bool
mustBurnAllHeadTokens mintValue headCurrencySymbol parties =
  traceIfFalse $(errorCode BurntTokenNumberMismatch) $
    burntTokens == L.length parties + 1
 where
  burntTokens =
    case AssocMap.lookup headCurrencySymbol (mintValueToMap mintValue) of
      Nothing -> 0
      Just tokenMap -> negate $ F.sum tokenMap
{-# INLINEABLE mustBurnAllHeadTokens #-}

mustNotMintOrBurn :: TxInfo -> Bool
mustNotMintOrBurn TxInfo{txInfoMint} =
  traceIfFalse "U01" $
    isZero (mintValueMinted txInfoMint) && isZero (mintValueBurned txInfoMint)
{-# INLINEABLE mustNotMintOrBurn #-}

-- | Hash a potentially unordered list of commits by sorting them, concatenating
-- their 'preSerializedOutput' bytes and creating a SHA2_256 digest over that.
--
-- NOTE: See note from `hashTxOuts`.
hashPreSerializedCommits :: [Commit] -> BuiltinByteString
hashPreSerializedCommits commits =
  sha2_256 . F.foldMap preSerializedOutput $
    L.sortBy (\a b -> compareRef (input a) (input b)) commits
{-# INLINEABLE hashPreSerializedCommits #-}

-- | Hash a pre-ordered list of transaction outputs by serializing each
-- individual 'TxOut', concatenating all bytes together and creating a SHA2_256
-- digest over that.
--
-- NOTE: In general, from asserting that `hash(x || y) = hash (x' || y')` it is
-- not safe to conclude that `(x,y) = (x', y')` as the same hash could be
-- obtained by moving one or more bytes from the end of `x` to the beginning of
-- `y`, but in the context of Hydra validators it seems impossible to exploit
-- this property without breaking other logic or verification (eg. producing a
-- valid and meaningful `TxOut`).
hashTxOuts :: [TxOut] -> BuiltinByteString
hashTxOuts =
  sha2_256 . F.foldMap (Builtins.serialiseData . toBuiltinData)
{-# INLINEABLE hashTxOuts #-}

compareRef :: TxOutRef -> TxOutRef -> Ordering
TxOutRef{txOutRefId, txOutRefIdx} `compareRef` TxOutRef{txOutRefId = id', txOutRefIdx = idx'} =
  case compare txOutRefId id' of
    EQ -> compare txOutRefIdx idx'
    ord -> ord
{-# INLINEABLE compareRef #-}

-- * Errors

data UtilError
  = MintingOrBurningIsForbidden
  deriving stock (Show)

instance ToErrorCode UtilError where
  toErrorCode = \case
    MintingOrBurningIsForbidden -> "U01"

-- | Get the list of 'TxOut' outputs of the pending transaction at
-- a given script address.
scriptOutputsAt :: ScriptHash -> TxInfo -> [(OutputDatum, Value)]
scriptOutputsAt h p =
  let flt TxOut{txOutDatum = d, txOutAddress = Address (ScriptCredential s) _, txOutValue} | s == h = Just (d, txOutValue)
      flt _ = Nothing
   in mapMaybe flt (txInfoOutputs p)
{-# INLINEABLE scriptOutputsAt #-}
