module Plutus.Codec.CBOR.EncodingSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Pretty as CBOR
import qualified Codec.CBOR.Write as CBOR
import Data.ByteArray (convert)
import Data.ByteString.Base16 (encodeBase16)
import Plutus.Codec.CBOR.Encoding (
  Encoding,
  encodeInteger,
  encodingToBuiltinByteString,
 )
import Test.QuickCheck (
  Property,
  choose,
  counterexample,
  forAll,
  oneof,
  (===),
 )

spec :: Spec
spec = do
  describe "Comparing CBOR encoding with oracle ('cborg' library)" $ do
    prop
      "works for all (x :: Integer) with 'encodeInteger'"
      propEncodeInteger

--
-- CBOR encoding against oracle
--

propCompareWithOracle ::
  Show x =>
  (x -> CBOR.Encoding) ->
  (x -> Encoding) ->
  x ->
  Property
propCompareWithOracle encodeOracle encodeOurs x =
  ( convert (CBOR.toStrictByteString oracle) === ours
  )
    & counterexample ("value: " <> show x)
    & counterexample ("\n─── cborg: \n" <> CBOR.prettyHexEnc oracle)
    & counterexample ("\n─── ours: \n" <> toString (encodeBase16 $ convert ours))
 where
  oracle = encodeOracle x
  ours = encodingToBuiltinByteString (encodeOurs x)

propEncodeInteger :: Property
propEncodeInteger =
  forAll genInteger $ propCompareWithOracle CBOR.encodeInteger encodeInteger

--
-- Generators
--

genInteger :: Gen Integer
genInteger =
  oneof
    [ genUnsignedInteger
    , negate <$> genUnsignedInteger
    ]
 where
  genUnsignedInteger =
    oneof
      [ choose (0, 24)
      , choose (24, 256)
      , choose (256, 65536)
      , choose (65536, 4294967296)
      , choose (4294967296, 18446744073709552000)
      ]
