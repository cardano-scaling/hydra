{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_HADDOCK prune #-}

module Plutus.Codec.CBOR.Encoding (
  -- * Encoding
  Encoding,
  encodingToBuiltinByteString,

  -- * Basic Types
  encodeInteger,
  encodeByteString,
  encodeNull,

  -- * Data-Structures

  -- ** Finite Structures
  encodeMaybe,
  encodeListLen,
  encodeList,
  encodeMapLen,
  encodeMap,

  -- ** Indefinite Structures
  encodeBreak,
  encodeBeginList,
  encodeListIndef,
  encodeBeginMap,
  encodeMapIndef,

  -- * Backdoor / Unsafe
  unsafeEncodeRaw,
) where

import PlutusTx.Prelude

import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Builtins (subtractInteger)

-- * Encoding

-- | An opaque 'Encoding' type. See also 'encodeToBuiltinByteString'.
newtype Encoding = Encoding (BuiltinByteString -> BuiltinByteString)

instance Semigroup Encoding where
  (Encoding a) <> (Encoding b) = Encoding (a . b)

instance Monoid Encoding where
  mempty = Encoding id

-- | Runs an encoder and produce a 'BuiltinByteString'.
encodingToBuiltinByteString :: Encoding -> BuiltinByteString
encodingToBuiltinByteString (Encoding runEncoder) =
  runEncoder emptyByteString
{-# INLINEABLE encodingToBuiltinByteString #-}

-- * Basic types

-- | Encode an 'Integer' as a CBOR type-00 or type-01 (negative) number.
--
-- Note (1): The 'Encoding' is of variable-length, larger numbers are larger to
-- encode.
--
-- Note (2): This can only encode numbers up to @2^64 - 1@ and down to @-2^63@
encodeInteger :: Integer -> Encoding
encodeInteger n
  | n < 0 =
    Encoding (encodeUnsigned 1 (subtractInteger 0 n - 1))
  | otherwise =
    Encoding (encodeUnsigned 0 n)
{-# INLINEABLE encodeInteger #-}

-- | Encode a 'BuiltinByteString' as a CBOR type-02 major type.
encodeByteString :: BuiltinByteString -> Encoding
encodeByteString bytes =
  Encoding (encodeUnsigned 2 (lengthOfByteString bytes) . appendByteString bytes)
{-# INLINEABLE encodeByteString #-}

-- | Encode a null character, useful to encode optional values.
encodeNull :: Encoding
encodeNull =
  Encoding (consByteString 246)
{-# INLINEABLE encodeNull #-}

-- * Data-Structure

-- | Encode a indefinite list or map termination. This must come (not
-- necessarily immediately) after an 'encodeListIndef' or 'encodeMapIndef'
encodeBreak :: Encoding
encodeBreak = Encoding (consByteString 0xFF)
{-# INLINEABLE encodeBreak #-}

-- | Declare a list of fixed size. Each element of the list must then be
-- separately provided via appending them ('Encoding' is a 'Semigroup').
--
-- This is useful to construct non-uniform arrays where elements may have
-- different types. For uniform list, see 'encodeList'.
--
-- @
-- -- Encoding (14, 42) as a finite list...
-- encodeListLen 2
--   <> encodeInteger 14
--   <> encodeInteger 42
-- @
encodeListLen :: Integer -> Encoding
encodeListLen = Encoding . encodeUnsigned 4
{-# INLINEABLE encodeListLen #-}

-- | Declare a list of indefinite size. Each element of the list must then be
-- separately provided via appending them ('Encoding' is a 'Semigroup').
--
-- This is useful to construct non-uniform arrays where elements may have
-- different types. For uniform list, see 'encodeListIndef'.
--
-- @
-- -- Encoding (14, 42) as an indefinite list...
-- encodeBeginList
--   <> encodeInteger 14
--   <> encodeInteger 42
--   <> encodeBreak
-- @
encodeBeginList :: Encoding
encodeBeginList = Encoding (withMajorType 4 31)
{-# INLINEABLE encodeBeginList #-}

-- | Shorthand for encoding a uniform list. Note that CBOR supports non-uniform
-- lists (i.e. n-tuples) for which one should use 'encodeListLen' or
-- 'encodeBeginList' / 'encodeBreak'.
encodeList :: (a -> Encoding) -> [a] -> Encoding
encodeList encodeElem =
  step 0 mempty
 where
  step n bs = \case
    [] -> encodeListLen n <> bs
    (e : q) -> step (n + 1) (bs <> encodeElem e) q
{-# INLINEABLE encodeList #-}

-- | Shorthand for encoding uniform list of indefinite sizes. Note that CBOR
-- supports non-uniform indefinite list (i.e. n-tuples) for which one should use
-- 'encodeListLen' or 'encodeBeginList' / 'encodeBreak'.
encodeListIndef :: (a -> Encoding) -> [a] -> Encoding
encodeListIndef encodeElem es =
  encodeBeginList <> step es
 where
  step = \case
    [] -> encodeBreak
    (e : q) -> encodeElem e <> step q
{-# INLINEABLE encodeListIndef #-}

-- | Declare a map of fixed size. Each key/value pair of the map must then
-- be separately provided via appending them ('Encoding' is a 'Semigroup').
--
-- This is useful to construct non-uniform maps where keys and values may have
-- different types. For uniform maps, see 'encodeMap'.
--
-- @
-- -- Encoding { 14: b'ffff', 42: b'0000' } as a finite map...
-- encodeMapLen 2
--   <> encodeInteger 14 <> encodeByteString "ffff"
--   <> encodeInteger 42 <> encodeByteString "0000"
-- @
encodeMapLen :: Integer -> Encoding
encodeMapLen = Encoding . encodeUnsigned 5
{-# INLINEABLE encodeMapLen #-}

-- | Declare a map of indefinite size. Each key/value pair of the map must then
-- be separately provided via appending them ('Encoding' is a 'Semigroup').
--
-- This is useful to construct non-uniform maps where keys and values may have
-- different types. For uniform maps, see 'encodeMap'.
--
-- @
-- -- Encoding { 14: b'ffff', 42: b'0000' } as a finite map...
-- encodeBeginMap
--   <> encodeInteger 14 <> encodeByteString "ffff"
--   <> encodeInteger 42 <> encodeByteString "0000"
--   <> encodeBreak
-- @
encodeBeginMap :: Encoding
encodeBeginMap = Encoding (withMajorType 5 31)
{-# INLINEABLE encodeBeginMap #-}

-- | Shorthand for encoding a uniform map of fixed size.
--
-- see also: 'encodeMapLen' / 'encodeBreak' for non-uniform maps.
encodeMap :: (k -> Encoding) -> (v -> Encoding) -> Map k v -> Encoding
encodeMap encodeKey encodeValue =
  step 0 mempty . Map.toList
 where
  step n bs = \case
    [] -> encodeMapLen n <> bs
    ((k, v) : q) -> step (n + 1) (bs <> encodeKey k <> encodeValue v) q
{-# INLINEABLE encodeMap #-}

-- | Shorthand for encoding a uniform map of indefinite size.
--
-- see also: 'encodeBeginMap' / 'encodeBreak' for non-uniform maps.
encodeMapIndef :: (k -> Encoding) -> (v -> Encoding) -> Map k v -> Encoding
encodeMapIndef encodeKey encodeValue m =
  encodeBeginMap <> step (Map.toList m)
 where
  step = \case
    [] -> encodeBreak
    ((k, v) : q) -> encodeKey k <> encodeValue v <> step q
{-# INLINEABLE encodeMapIndef #-}

-- | Helper for optionally encoding a type. Note that in the @Nothing@ case,
-- this is a no-op.
encodeMaybe :: (a -> Encoding) -> Maybe a -> Encoding
encodeMaybe encode = \case
  Nothing -> Encoding id
  Just a -> encode a
{-# INLINEABLE encodeMaybe #-}

-- * Backdoor

-- | Inject an already CBOR-encoded bytestring into an 'Encoding'. Do not use
-- unless you know what you're doing, this may creates an 'Encoding' not
-- compliant with the CBOR specification.
unsafeEncodeRaw :: BuiltinByteString -> Encoding
unsafeEncodeRaw =
  Encoding . appendByteString
{-# INLINEABLE unsafeEncodeRaw #-}

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
