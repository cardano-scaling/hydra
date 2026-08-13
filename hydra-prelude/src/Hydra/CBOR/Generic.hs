{-# LANGUAGE DataKinds #-}

-- | Generic derivation of 'ToCBOR' / 'FromCBOR' instances in the
-- constructor-name-tagged format used for hydra-node persistence and API
-- messages.
--
-- The encoding of a value is the constructor name as a CBOR text string,
-- followed by the encodings of the constructor fields in declaration order,
-- concatenated without any list framing:
--
-- @
-- toCBOR ("ConstructorName" :: Text) <> toCBOR field1 <> toCBOR field2 <> ...
-- @
--
-- Every constructor is tagged, including the single constructor of records
-- and newtypes. Tagging by name (instead of by declaration index like the
-- @serialise@ package) means adding or reordering constructors does not
-- change the encoding of existing data; removing or renaming a constructor,
-- or changing the order or type of its fields, does and requires a migration
-- of persisted data.
--
-- Since the field encodings come from the data type declaration, the
-- declaration itself becomes the wire format: reordering record fields is a
-- format change that the type checker will not flag. Golden tests (see
-- 'Hydra.CBORSpec' in hydra-node) are the guard against that.
--
-- Decoding fails with @"\<tag\>" is not a proper CBOR-encoded \<TypeName\>@
-- when the decoded tag matches no constructor.
module Hydra.CBOR.Generic (
  genericToCBOR,
  genericFromCBOR,
) where

import Relude

import Cardano.Binary (Decoder, Encoding, FromCBOR (..), ToCBOR (..))
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)

-- | Encode a value in the constructor-name-tagged CBOR format:
--
-- @
-- instance ToCBOR MyType where
--   toCBOR = genericToCBOR
-- @
genericToCBOR :: (Generic a, GToCBOR (Rep a)) => a -> Encoding
genericToCBOR = gToCBOR . from

-- | Decode a value from the constructor-name-tagged CBOR format produced by
-- 'genericToCBOR':
--
-- @
-- instance FromCBOR MyType where
--   fromCBOR = genericFromCBOR
-- @
genericFromCBOR :: (Generic a, GFromCBOR (Rep a)) => Decoder s a
genericFromCBOR = to <$> gFromCBOR

-- * Encoding

class GToCBOR f where
  gToCBOR :: f p -> Encoding

instance GToCBOR f => GToCBOR (D1 meta f) where
  gToCBOR (M1 x) = gToCBOR x

instance (GToCBOR f, GToCBOR g) => GToCBOR (f :+: g) where
  gToCBOR = \case
    L1 x -> gToCBOR x
    R1 x -> gToCBOR x

instance (KnownSymbol name, GFieldsToCBOR f) => GToCBOR (C1 ('MetaCons name fixity hasSelectors) f) where
  gToCBOR (M1 x) = toCBOR (conNameText (Proxy @name)) <> gFieldsToCBOR x

class GFieldsToCBOR f where
  gFieldsToCBOR :: f p -> Encoding

instance GFieldsToCBOR U1 where
  gFieldsToCBOR U1 = mempty

instance (GFieldsToCBOR f, GFieldsToCBOR g) => GFieldsToCBOR (f :*: g) where
  gFieldsToCBOR (x :*: y) = gFieldsToCBOR x <> gFieldsToCBOR y

instance GFieldsToCBOR f => GFieldsToCBOR (S1 meta f) where
  gFieldsToCBOR (M1 x) = gFieldsToCBOR x

instance ToCBOR a => GFieldsToCBOR (K1 i a) where
  gFieldsToCBOR (K1 a) = toCBOR a

-- * Decoding

class GFromCBOR f where
  gFromCBOR :: Decoder s (f p)

instance (KnownSymbol typeName, GConsFromCBOR f) => GFromCBOR (D1 ('MetaData typeName moduleName package isNewtype) f) where
  gFromCBOR = do
    tag <- fromCBOR
    case gConsFromCBOR tag of
      Just decodeFields -> M1 <$> decodeFields
      Nothing ->
        fail $ show tag <> " is not a proper CBOR-encoded " <> symbolVal (Proxy @typeName)

class GConsFromCBOR f where
  -- | The field decoder of the constructor matching the given tag, if any.
  gConsFromCBOR :: Text -> Maybe (Decoder s (f p))

instance (GConsFromCBOR f, GConsFromCBOR g) => GConsFromCBOR (f :+: g) where
  gConsFromCBOR tag =
    (fmap L1 <$> gConsFromCBOR tag) <|> (fmap R1 <$> gConsFromCBOR tag)

instance (KnownSymbol name, GFieldsFromCBOR f) => GConsFromCBOR (C1 ('MetaCons name fixity hasSelectors) f) where
  gConsFromCBOR tag
    | tag == conNameText (Proxy @name) = Just (M1 <$> gFieldsFromCBOR)
    | otherwise = Nothing

class GFieldsFromCBOR f where
  gFieldsFromCBOR :: Decoder s (f p)

instance GFieldsFromCBOR U1 where
  gFieldsFromCBOR = pure U1

instance (GFieldsFromCBOR f, GFieldsFromCBOR g) => GFieldsFromCBOR (f :*: g) where
  gFieldsFromCBOR = (:*:) <$> gFieldsFromCBOR <*> gFieldsFromCBOR

instance GFieldsFromCBOR f => GFieldsFromCBOR (S1 meta f) where
  gFieldsFromCBOR = M1 <$> gFieldsFromCBOR

instance FromCBOR a => GFieldsFromCBOR (K1 i a) where
  gFieldsFromCBOR = K1 <$> fromCBOR

-- | The constructor name from generic metadata, as 'Text'.
conNameText :: KnownSymbol name => Proxy name -> Text
conNameText = toText . symbolVal
