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

-- | A baseline validator which does nothing but returning 'True'. We use it as
-- baseline to measure the deviation for cost execution of other validators.
data EmptyValidator

instance Scripts.ValidatorTypes EmptyValidator where
  type DatumType EmptyValidator = ()
  type RedeemerType EmptyValidator = ()

emptyValidator :: Scripts.TypedValidator EmptyValidator
emptyValidator =
  Scripts.mkTypedValidator @EmptyValidator
    $$(Plutus.compile [||\() () _ctx -> lengthOfByteString "" == 0||])
    $$(Plutus.compile [||wrap||])
 where
  wrap = Scripts.wrapValidator @() @()

-- | A validator for measuring cost of encoding values. The validator is
-- parameterized by the type of value.
data EncodeValidator a

instance Scripts.ValidatorTypes (EncodeValidator a) where
  type DatumType (EncodeValidator a) = ()
  type RedeemerType (EncodeValidator a) = a

encodeIntegerValidator :: Scripts.TypedValidator (EncodeValidator Integer)
encodeIntegerValidator =
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

encodeByteStringValidator :: Scripts.TypedValidator (EncodeValidator BuiltinByteString)
encodeByteStringValidator =
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

encodeListValidator :: Scripts.TypedValidator (EncodeValidator [BuiltinByteString])
encodeListValidator =
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

encodeMapValidator :: Scripts.TypedValidator (EncodeValidator (Map BuiltinByteString BuiltinByteString))
encodeMapValidator =
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

encodeTxOutValidator :: Scripts.TypedValidator (EncodeValidator TxOut)
encodeTxOutValidator =
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
