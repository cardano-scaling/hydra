-- | Helpers to test 'ToCBOR' / 'FromCBOR' instances.
module Test.Hydra.CBOR where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Data.Typeable (typeRep)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import Test.QuickCheck (Property, forAll, resize, withMaxSuccess, (===))
import Test.QuickCheck.Arbitrary.ADT (ADTArbitrary (..), ConstructorArbitraryPair (..), ToADTArbitrary, toADTArbitrary)

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
roundtripCBOR p = do
  prop ("roundtrips CBOR encoding: " <> show (typeRep p)) $
    -- Small samples exercise the codecs (tags, field order) just as well as
    -- large ones, and generation cost is dominated by expensive embedded
    -- structures (full transactions, KZG accumulators): unsized, the
    -- Snapshot roundtrip alone took ~24 minutes of CI time.
    forAll (resize 5 (arbitrary @a)) prop_canRoundtripCBOREncoding
  -- Nightly keeps coverage of size-dependent encodings (e.g. CBOR
  -- length-boundary widths) that small samples cannot reach.
  around_ onlyNightly $
    prop ("roundtrips CBOR encoding (large samples): " <> show (typeRep p) <> " @nightly") $
      withMaxSuccess 20 $
        prop_canRoundtripCBOREncoding @a

-- | Golden test locking a persisted CBOR format. The golden file holds the
-- raw CBOR of a list of samples. When the file is missing it is created from
-- the given generator with a fixed seed — commit the result. On every other
-- run the stored bytes must decode successfully and re-encode to the exact
-- same bytes.
--
-- This catches codec changes that would break decoding of already persisted
-- data (e.g. hydra.db events) — including symmetric encoder+decoder drift
-- (say, reordering the fields on both sides) that roundtrip properties
-- cannot see. If this fails, the change breaks existing databases and needs
-- a schema migration; only delete and regenerate the golden file alongside
-- one.
-- | One sample per constructor of @a@, in declaration order, enumerated
-- generically by 'ToADTArbitrary': coverage of every constructor holds by
-- construction and new constructors are included automatically. Samples are
-- generated small (resized): golden files lock tags and field order, which
-- small values exercise just as well.
genGoldenSamples :: forall a. ToADTArbitrary a => Gen [a]
genGoldenSamples = do
  ADTArbitrary{adtCAPs} <- resize 5 $ toADTArbitrary (Proxy @a)
  pure $ capArbitrary <$> adtCAPs

-- | A single golden sample via the type's own 'Arbitrary' instance. Use for
-- single-constructor types (newtypes, records) where the per-constructor
-- enumeration of 'genGoldenSamples' adds nothing, or where the generic
-- field-wise generation of 'ToADTArbitrary' is not applicable.
genGoldenSample :: Arbitrary a => Gen [a]
genGoldenSample = (: []) <$> resize 5 arbitrary

goldenCBOR ::
  forall a.
  (ToCBOR a, FromCBOR a) =>
  String ->
  FilePath ->
  Gen [a] ->
  Spec
goldenCBOR name path gen =
  it ("golden CBOR: " <> name) $ do
    unlessM (doesFileExist path) $ do
      createDirectoryIfMissing True (takeDirectory path)
      writeFileBS path . serialize' $ generateWith gen 42
    bytes <- readFileBS path
    case decodeFull' @[a] bytes of
      Left err ->
        expectationFailure $
          "failed to decode golden file " <> path <> ": " <> show err
      Right samples -> serialize' samples `shouldBe` bytes
