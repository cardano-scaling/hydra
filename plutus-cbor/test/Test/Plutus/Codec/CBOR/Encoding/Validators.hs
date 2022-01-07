{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Test.Plutus.Codec.CBOR.Encoding.Validators where

import PlutusTx.Prelude

import qualified Ledger.Typed.Scripts as Scripts
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
  ValidatorHash (..),
  Value (..),
 )
import qualified PlutusTx as Plutus
import PlutusTx.AssocMap (Map)

-- | A validator for measuring cost of encoding values. The validator is
-- parameterized by the type of value.
data EncodeValidator a

data ValidatorKind = BaselineValidator | RealValidator

Plutus.unstableMakeIsData ''ValidatorKind

instance Scripts.ValidatorTypes (EncodeValidator a) where
  type DatumType (EncodeValidator a) = ()
  type RedeemerType (EncodeValidator a) = a

encodeIntegerValidator :: ValidatorKind -> Scripts.TypedValidator (EncodeValidator Integer)
encodeIntegerValidator = \case
  BaselineValidator ->
    Scripts.mkTypedValidator @(EncodeValidator Integer)
      $$(Plutus.compile [||\() _ _ctx -> True||])
      $$(Plutus.compile [||wrap||])
  RealValidator ->
    Scripts.mkTypedValidator @(EncodeValidator Integer)
      $$( Plutus.compile
            [||
            \() a _ctx ->
              let bytes = encodingToBuiltinByteString (encodeInteger a)
               in lengthOfByteString bytes > 0
            ||]
        )
      $$(Plutus.compile [||wrap||])
 where
  wrap = Scripts.wrapValidator @() @Integer

encodeByteStringValidator :: ValidatorKind -> Scripts.TypedValidator (EncodeValidator BuiltinByteString)
encodeByteStringValidator = \case
  BaselineValidator ->
    Scripts.mkTypedValidator @(EncodeValidator BuiltinByteString)
      $$(Plutus.compile [||\() _ _ctx -> True||])
      $$(Plutus.compile [||wrap||])
  RealValidator ->
    Scripts.mkTypedValidator @(EncodeValidator BuiltinByteString)
      $$( Plutus.compile
            [||
            \() a _ctx ->
              let bytes = encodingToBuiltinByteString (encodeByteString a)
               in lengthOfByteString bytes > 0
            ||]
        )
      $$(Plutus.compile [||wrap||])
 where
  wrap = Scripts.wrapValidator @() @BuiltinByteString

encodeListValidator :: ValidatorKind -> Scripts.TypedValidator (EncodeValidator [BuiltinByteString])
encodeListValidator = \case
  BaselineValidator ->
    Scripts.mkTypedValidator @(EncodeValidator [BuiltinByteString])
      $$(Plutus.compile [||\() _ _ctx -> True||])
      $$(Plutus.compile [||wrap||])
  RealValidator ->
    Scripts.mkTypedValidator @(EncodeValidator [BuiltinByteString])
      $$( Plutus.compile
            [||
            \() xs _ctx ->
              let bytes =
                    encodingToBuiltinByteString $
                      encodeList encodeByteString xs
               in lengthOfByteString bytes > 0
            ||]
        )
      $$(Plutus.compile [||wrap||])
 where
  wrap = Scripts.wrapValidator @() @[BuiltinByteString]

encodeMapValidator :: ValidatorKind -> Scripts.TypedValidator (EncodeValidator (Map BuiltinByteString BuiltinByteString))
encodeMapValidator = \case
  BaselineValidator ->
    Scripts.mkTypedValidator @(EncodeValidator (Map BuiltinByteString BuiltinByteString))
      $$(Plutus.compile [||\() _ _ctx -> True||])
      $$(Plutus.compile [||wrap||])
  RealValidator ->
    Scripts.mkTypedValidator @(EncodeValidator (Map BuiltinByteString BuiltinByteString))
      $$( Plutus.compile
            [||
            \() m _ctx ->
              let bytes =
                    encodingToBuiltinByteString $
                      encodeMap encodeByteString encodeByteString m
               in lengthOfByteString bytes > 0
            ||]
        )
      $$(Plutus.compile [||wrap||])
 where
  wrap = Scripts.wrapValidator @() @(Map BuiltinByteString BuiltinByteString)

encodeTxOutValidator :: ValidatorKind -> Scripts.TypedValidator (EncodeValidator TxOut)
encodeTxOutValidator = \case
  BaselineValidator ->
    Scripts.mkTypedValidator @(EncodeValidator TxOut)
      $$(Plutus.compile [||\() _ _ctx -> True||])
      $$(Plutus.compile [||wrap||])
  RealValidator ->
    Scripts.mkTypedValidator @(EncodeValidator TxOut)
      $$( Plutus.compile
            [||
            \() o _ctx ->
              let bytes = encodingToBuiltinByteString (encodeTxOut o)
               in lengthOfByteString bytes > 0
            ||]
        )
      $$(Plutus.compile [||wrap||])
 where
  wrap = Scripts.wrapValidator @() @TxOut

  encodeTxOut :: TxOut -> Encoding
  encodeTxOut (TxOut addr value datum) =
    encodeListLen 3
      <> encodeAddress addr
      <> encodeValue value
      <> encodeDatum datum

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

  encodeValue :: Value -> Encoding
  encodeValue =
    encodeMap encodeCurrencySymbol (encodeMap encodeTokenName encodeInteger) . getValue
   where
    encodeCurrencySymbol (CurrencySymbol symbol) = encodeByteString symbol
    encodeTokenName (TokenName token) = encodeByteString token

  encodeDatum :: Maybe DatumHash -> Encoding
  encodeDatum =
    encodeMaybe (\(DatumHash h) -> encodeByteString h)
