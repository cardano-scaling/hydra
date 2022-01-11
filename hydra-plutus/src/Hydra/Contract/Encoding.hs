{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Encoding where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import Plutus.Codec.CBOR.Encoding (
  Encoding,
  encodeBeginList,
  encodeBreak,
  encodeByteString,
  encodeInteger,
  encodeListLen,
  encodeMap,
  encodeMapIndef,
  encodingToBuiltinByteString,
 )
import Plutus.V1.Ledger.Api (
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol (CurrencySymbol),
  StakingCredential (..),
  TokenName (TokenName),
  adaSymbol,
  adaToken,
  getValue,
 )
import Plutus.V1.Ledger.Value (valueOf)
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as Map

serialiseTxOuts :: [TxOut] -> BuiltinByteString
serialiseTxOuts outs =
  encodingToBuiltinByteString $
    encodeBeginList <> foldMap encodeTxOut outs <> encodeBreak
{-# INLINEABLE serialiseTxOuts #-}

encodeTxOut :: TxOut -> Encoding
encodeTxOut TxOut{txOutAddress, txOutValue, txOutDatumHash} =
  case txOutDatumHash of
    Just h ->
      encodeListLen 3
        <> encodeAddress txOutAddress
        <> encodeValue txOutValue
        <> encodeDatum h
    Nothing ->
      encodeListLen 2
        <> encodeAddress txOutAddress
        <> encodeValue txOutValue

-- See for details: https://github.com/cardano-foundation/CIPs/blob/master/CIP-0019/CIP-0019-cardano-addresses.abnf
--
-- FIXME: This currently assumes that all addresses are TESTNET addresses.
-- Eventually, we need to pass the network id as argument and add `1` to the
-- address prefixes.
encodeAddress :: Address -> Encoding
encodeAddress Address{addressCredential, addressStakingCredential} =
  encodeByteString bytes
 where
  bytes =
    case (addressCredential, addressStakingCredential) of
      (PubKeyCredential (PubKeyHash h), Just (StakingHash (PubKeyCredential (PubKeyHash h')))) ->
        addrType00 `consByteString` (h `appendByteString` h')
      (ScriptCredential (ValidatorHash h), Just (StakingHash (PubKeyCredential (PubKeyHash h')))) ->
        addrType01 `consByteString` (h `appendByteString` h')
      (PubKeyCredential (PubKeyHash h), Just (StakingHash (ScriptCredential (ValidatorHash h')))) ->
        addrType02 `consByteString` (h `appendByteString` h')
      (ScriptCredential (ValidatorHash h), Just (StakingHash (ScriptCredential (ValidatorHash h')))) ->
        addrType03 `consByteString` (h `appendByteString` h')
      (PubKeyCredential (PubKeyHash h), Nothing) ->
        addrType06 `consByteString` h
      (ScriptCredential (ValidatorHash h), Nothing) ->
        addrType07 `consByteString` h
      _ ->
        traceError "encodeAddress: ptr"
  addrType00 = 0
  addrType01 = 16
  addrType02 = 32
  addrType03 = 48
  addrType06 = 96
  addrType07 = 112

encodeValue :: Value -> Encoding
encodeValue val =
  if Map.null assets
    then encodeInteger coins
    else encodeListLen 2 <> encodeInteger coins <> encodeAssets assets
 where
  coins = valueOf val adaSymbol adaToken
  assets = Map.delete adaSymbol (getValue val)
{-# INLINEABLE encodeValue #-}

encodeAssets :: Map CurrencySymbol (Map TokenName Integer) -> Encoding
encodeAssets m
  | length m <= 23 = encodeMap encodeCurrencySymbol encodeSingleAsset m
  | otherwise = encodeMapIndef encodeCurrencySymbol encodeSingleAsset m
{-# INLINEABLE encodeAssets #-}

encodeSingleAsset :: Map TokenName Integer -> Encoding
encodeSingleAsset m
  | length m <= 23 = encodeMap encodeTokenName encodeInteger m
  | otherwise = encodeMapIndef encodeTokenName encodeInteger m
{-# INLINEABLE encodeSingleAsset #-}

encodeCurrencySymbol :: CurrencySymbol -> Encoding
encodeCurrencySymbol (CurrencySymbol symbol) =
  encodeByteString symbol
{-# INLINEABLE encodeCurrencySymbol #-}

encodeTokenName :: TokenName -> Encoding
encodeTokenName (TokenName token) =
  encodeByteString token
{-# INLINEABLE encodeTokenName #-}

encodeDatum :: DatumHash -> Encoding
encodeDatum (DatumHash h) = encodeByteString h
{-# INLINEABLE encodeDatum #-}
