{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Plutus.Codec.CBOR.Encoding.Validator where

import PlutusTx.Prelude

import Plutus.Codec.CBOR.Encoding (
  Encoding,
  encodeByteString,
  encodeInteger,
  encodeList,
  encodeListLen,
  encodeMap,
  encodeMaybe,
  encodingToBuiltinByteString,
 )
import Plutus.V1.Ledger.Api (
  Address (..),
  Credential (..),
  CurrencySymbol (..),
  DatumHash (..),
  PubKeyHash (..),
  TokenName (..),
  TxOut (..),
  Validator,
  ValidatorHash (..),
  Value (..),
  mkValidatorScript,
 )
import qualified PlutusTx as Plutus
import Test.Plutus.Validator (wrapValidator)

-- | A validator for measuring cost of encoding values. The validator is
-- parameterized by the type of value.
data EncodeValidator a

data ValidatorKind = BaselineValidator | RealValidator

Plutus.unstableMakeIsData ''ValidatorKind

encodeIntegerValidator :: ValidatorKind -> Validator
encodeIntegerValidator = \case
  BaselineValidator ->
    mkValidatorScript $$(Plutus.compile [||wrapValidator $ \() (_ :: Integer) _ctx -> True||])
  RealValidator ->
    mkValidatorScript
      $$( Plutus.compile
            [||
            wrapValidator $ \() a _ctx ->
              let bytes = encodingToBuiltinByteString (encodeInteger a)
               in lengthOfByteString bytes > 0
            ||]
        )

encodeByteStringValidator :: ValidatorKind -> Validator
encodeByteStringValidator = \case
  BaselineValidator ->
    mkValidatorScript
      $$(Plutus.compile [||wrapValidator $ \() (_ :: BuiltinByteString) _ctx -> True||])
  RealValidator ->
    mkValidatorScript
      $$( Plutus.compile
            [||
            wrapValidator $ \() a _ctx ->
              let bytes = encodingToBuiltinByteString (encodeByteString a)
               in lengthOfByteString bytes > 0
            ||]
        )

encodeListValidator :: ValidatorKind -> Validator
encodeListValidator = \case
  BaselineValidator ->
    mkValidatorScript
      $$(Plutus.compile [||wrapValidator $ \() (_ :: [BuiltinByteString]) _ctx -> True||])
  RealValidator ->
    mkValidatorScript
      $$( Plutus.compile
            [||
            wrapValidator $ \() xs _ctx ->
              let bytes =
                    encodingToBuiltinByteString $
                      encodeList encodeByteString xs
               in lengthOfByteString bytes > 0
            ||]
        )

encodeTxOutValidator :: ValidatorKind -> Validator
encodeTxOutValidator = \case
  BaselineValidator ->
    mkValidatorScript
      $$(Plutus.compile [||wrapValidator $ \() (_ :: TxOut) _ctx -> True||])
  RealValidator ->
    mkValidatorScript
      $$( Plutus.compile
            [||
            wrapValidator $ \() o _ctx ->
              let bytes = encodingToBuiltinByteString (encodeTxOut o)
               in lengthOfByteString bytes > 0
            ||]
        )

encodeTxOutsValidator :: ValidatorKind -> Validator
encodeTxOutsValidator = \case
  BaselineValidator ->
    mkValidatorScript
      $$(Plutus.compile [||wrapValidator $ \() (_ :: [TxOut]) _ctx -> True||])
  RealValidator ->
    mkValidatorScript
      $$( Plutus.compile
            [||
            wrapValidator $ \() xs _ctx ->
              let bytes = encodingToBuiltinByteString (encodeList encodeTxOut xs)
               in lengthOfByteString bytes > 0
            ||]
        )

encodeTxOut :: TxOut -> Encoding
encodeTxOut (TxOut addr value datum) =
  encodeListLen 3
    <> encodeAddress addr
    <> encodeValue value
    <> encodeDatum datum
{-# INLINEABLE encodeTxOut #-}

-- NOTE 1: This is missing the header byte with network discrimination. For the
-- sake of getting an order of magnitude and moving forward, it is fine.
--
-- NOTE 2: This is ignoring any stake reference and assuming that all addresses
-- are plain script or payment addresses with no delegation whatsoever. Again,
-- see NOTE #1.
encodeAddress :: Address -> Encoding
encodeAddress Address{addressCredential} =
  encodeByteString (credentialToBytes addressCredential)
 where
  credentialToBytes = \case
    PubKeyCredential (PubKeyHash h) -> h
    ScriptCredential (ValidatorHash h) -> h
{-# INLINEABLE encodeAddress #-}

encodeValue :: Value -> Encoding
encodeValue =
  encodeMap encodeCurrencySymbol (encodeMap encodeTokenName encodeInteger) . getValue
 where
  encodeCurrencySymbol (CurrencySymbol symbol) = encodeByteString symbol
  encodeTokenName (TokenName token) = encodeByteString token
{-# INLINEABLE encodeValue #-}

encodeDatum :: Maybe DatumHash -> Encoding
encodeDatum =
  encodeMaybe (\(DatumHash h) -> encodeByteString h)
{-# INLINEABLE encodeDatum #-}
