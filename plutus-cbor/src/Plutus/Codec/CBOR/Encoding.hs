module Plutus.Codec.CBOR.Encoding (
  Encoding,
  encodingToBuiltinByteString,
  encodeInteger,
) where

import PlutusTx.Prelude

import PlutusTx.Builtins (subtractInteger)

-- * Encoding

newtype Encoding = Encoding BuiltinByteString

encodingToBuiltinByteString :: Encoding -> BuiltinByteString
encodingToBuiltinByteString (Encoding bytes) = bytes
{-# INLINEABLE encodingToBuiltinByteString #-}

-- * Basic types

encodeInteger :: Integer -> Encoding
encodeInteger n
  | n < 0 =
    encodeUnsigned 1 (subtractInteger 0 n - 1)
  | otherwise =
    encodeUnsigned 0 n
{-# INLINEABLE encodeInteger #-}

-- * Internal

withMajorType :: Integer -> Integer -> Encoding -> Encoding
withMajorType major n =
  consByteString (32 * major + n)
{-# INLINEABLE withMajorType #-}

encodeUnsigned :: Integer -> Integer -> Encoding
encodeUnsigned major n
  | n < 24 =
    withMajorType major n emptyByteString
  | n < 256 =
    withMajorType major 24 (encodeUnsigned8 n)
  | n < 65536 =
    withMajorType major 25 (encodeUnsigned16 n)
  | n < 4294967296 =
    withMajorType major 26 (encodeUnsigned32 n)
  | otherwise =
    withMajorType major 27 (encodeUnsigned64 n)
{-# INLINEABLE encodeUnsigned #-}

encodeUnsigned8 :: Integer -> Encoding
encodeUnsigned8 n =
  consByteString n emptyByteString
{-# INLINEABLE encodeUnsigned8 #-}

encodeUnsigned16 :: Integer -> Encoding
encodeUnsigned16 n =
  appendByteString
    (encodeUnsigned8 (quotient n 256))
    (encodeUnsigned8 (remainder n 256))
{-# INLINEABLE encodeUnsigned16 #-}

encodeUnsigned32 :: Integer -> Encoding
encodeUnsigned32 n =
  appendByteString
    (encodeUnsigned16 (quotient n 65536))
    (encodeUnsigned16 (remainder n 65536))
{-# INLINEABLE encodeUnsigned32 #-}

encodeUnsigned64 :: Integer -> Encoding
encodeUnsigned64 n =
  appendByteString
    (encodeUnsigned32 (quotient n 4294967296))
    (encodeUnsigned32 (remainder n 4294967296))
{-# INLINEABLE encodeUnsigned64 #-}
