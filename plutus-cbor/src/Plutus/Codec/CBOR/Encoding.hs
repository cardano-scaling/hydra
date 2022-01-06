{-# OPTIONS_GHC -fno-specialize #-}

module Plutus.Codec.CBOR.Encoding (
  Encoding,
  encodingToBuiltinByteString,
  encodeInteger,
  encodeByteString,
  encodeList,
) where

import PlutusTx.Prelude

import PlutusTx.Builtins (subtractInteger)

-- * Encoding

newtype Encoding = Encoding (BuiltinByteString -> BuiltinByteString)

encodingToBuiltinByteString :: Encoding -> BuiltinByteString
encodingToBuiltinByteString (Encoding runEncoder) =
  runEncoder emptyByteString
{-# INLINEABLE encodingToBuiltinByteString #-}

-- * Basic types

encodeInteger :: Integer -> Encoding
encodeInteger n
  | n < 0 =
    Encoding (encodeUnsigned 1 (subtractInteger 0 n - 1))
  | otherwise =
    Encoding (encodeUnsigned 0 n)
{-# INLINEABLE encodeInteger #-}

encodeByteString :: BuiltinByteString -> Encoding
encodeByteString bytes =
  Encoding (encodeUnsigned 2 (lengthOfByteString bytes) . appendByteString bytes)
{-# INLINEABLE encodeByteString #-}

-- * Data-Structure

encodeList :: [Encoding] -> Encoding
encodeList xs =
  Encoding $ \next ->
    encodeUnsigned 4 (length xs) $
      foldr
        (\(Encoding runEncoder) bytes -> runEncoder bytes)
        next
        xs
{-# INLINEABLE encodeList #-}

-- * Internal

withMajorType :: Integer -> Integer -> BuiltinByteString -> BuiltinByteString
withMajorType major n =
  consByteString (32 * major + n)
{-# INLINEABLE withMajorType #-}

encodeUnsigned :: Integer -> Integer -> BuiltinByteString -> BuiltinByteString
encodeUnsigned major n next
  | n < 24 =
    withMajorType major n next
  | n < 256 =
    withMajorType major 24 (encodeUnsigned8 n next)
  | n < 65536 =
    withMajorType major 25 (encodeUnsigned16 n next)
  | n < 4294967296 =
    withMajorType major 26 (encodeUnsigned32 n next)
  | otherwise =
    withMajorType major 27 (encodeUnsigned64 n next)
{-# INLINEABLE encodeUnsigned #-}

encodeUnsigned8 :: Integer -> BuiltinByteString -> BuiltinByteString
encodeUnsigned8 = consByteString
{-# INLINEABLE encodeUnsigned8 #-}

encodeUnsigned16 :: Integer -> BuiltinByteString -> BuiltinByteString
encodeUnsigned16 n =
  encodeUnsigned8 (quotient n 256) . encodeUnsigned8 (remainder n 256)
{-# INLINEABLE encodeUnsigned16 #-}

encodeUnsigned32 :: Integer -> BuiltinByteString -> BuiltinByteString
encodeUnsigned32 n =
  encodeUnsigned16 (quotient n 65536) . encodeUnsigned16 (remainder n 65536)
{-# INLINEABLE encodeUnsigned32 #-}

encodeUnsigned64 :: Integer -> BuiltinByteString -> BuiltinByteString
encodeUnsigned64 n =
  encodeUnsigned32 (quotient n 4294967296) . encodeUnsigned32 (remainder n 4294967296)
{-# INLINEABLE encodeUnsigned64 #-}
