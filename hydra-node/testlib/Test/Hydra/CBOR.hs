-- | Helpers to test 'ToCBOR' / 'FromCBOR' instances.
module Test.Hydra.CBOR where

import Hydra.Prelude
import Test.Hydra.Prelude

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Data.Typeable (typeRep)
import Test.QuickCheck (Property, (===))

-- | Test that a value can be roundtripped through its CBOR encoding.
prop_canRoundtripCBOREncoding ::
  (ToCBOR a, FromCBOR a, Eq a, Show a) => a -> Property
prop_canRoundtripCBOREncoding a =
  let encoded = toLazyByteString $ toCBOR a
   in (snd <$> deserialiseFromBytes fromCBOR encoded) === Right a

-- | A property spec asserting CBOR roundtrips for arbitrary values of @a@.
-- This is the CI guard that keeps the hand-written encoder/decoder pairs in
-- sync: a new constructor without a (correct) codec fails here.
roundtripCBOR ::
  forall a.
  (Arbitrary a, ToCBOR a, FromCBOR a, Eq a, Show a) =>
  Proxy a ->
  Spec
roundtripCBOR p =
  prop ("roundtrips CBOR encoding: " <> show (typeRep p)) $
    prop_canRoundtripCBOREncoding @a
