{-# LANGUAGE TypeApplications #-}

module Plutus.Codec.CBOR.EncodingSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Pretty as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encodeBase16)
import Plutus.Codec.CBOR.Encoding (
  Encoding,
  encodeByteString,
  encodeInteger,
  encodeList,
  encodeListIndef,
  encodeMap,
  encodeMapIndef,
  encodeNull,
  encodingToBuiltinByteString,
 )
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified PlutusTx.AssocMap as Plutus.Map
import Test.QuickCheck (
  Property,
  choose,
  counterexample,
  elements,
  forAllShrink,
  liftShrink2,
  oneof,
  shrinkList,
  vector,
  vectorOf,
  withMaxSuccess,
  (===),
 )
import qualified Prelude

spec :: Spec
spec = do
  describe "our CBOR encoding matches oracle's ('cborg' library)" $ do
    prop "for all values" $
      withMaxSuccess 1000 $
        forAllShrink genSomeValue shrinkSomeValue $ \(SomeValue x _ encode encode') ->
          propCompareWithOracle encode encode' x

-- | Compare encoding a value 'x' with our own encoder and a reference
-- implementation. Counterexamples shows both encoded values, but in a pretty /
-- readable form for the reference implementation.
propCompareWithOracle ::
  Show x =>
  (x -> CBOR.Encoding) ->
  (x -> Encoding) ->
  x ->
  Property
propCompareWithOracle encodeOracle encodeOurs x =
  ( Plutus.toBuiltin (CBOR.toStrictByteString oracle) === ours
  )
    & counterexample ("value: " <> show x)
    & counterexample ("\n─── cborg: \n" <> CBOR.prettyHexEnc oracle)
    & counterexample ("\n─── ours: \n" <> toString (encodeBase16 $ Plutus.fromBuiltin ours))
 where
  oracle = encodeOracle x
  ours = encodingToBuiltinByteString (encodeOurs x)

--
-- SomeValue
--

data SomeValue where
  SomeValue ::
    forall a.
    Show a =>
    a ->
    (a -> [a]) ->
    (a -> CBOR.Encoding) ->
    (a -> Encoding) ->
    SomeValue

instance Prelude.Show SomeValue where
  show (SomeValue val _ _ _) = show val

shrinkSomeValue :: SomeValue -> [SomeValue]
shrinkSomeValue (SomeValue a shrinker encode encode') =
  fmap (\a' -> SomeValue a' shrinker encode encode') (shrinker a)

genSomeValue :: Gen SomeValue
genSomeValue =
  withMaxDepth 1
 where
  withMaxDepth :: Int -> Gen SomeValue
  withMaxDepth n =
    oneof $
      catMaybes
        [ Just genSomeInteger
        , Just genSomeByteString
        , Just genSomeNull
        , guard (n > 0) $> genSomeList n
        , guard (n > 0) $> genSomeMap n
        , guard (n > 0) $> genSomeListIndef n
        , guard (n > 0) $> genSomeMapIndef n
        ]

  genSomeInteger :: Gen SomeValue
  genSomeInteger = do
    val <- genInteger
    return $ SomeValue val shrink CBOR.encodeInteger encodeInteger

  genSomeByteString :: Gen SomeValue
  genSomeByteString = do
    val <- genByteString
    return $ SomeValue val shrinkByteString CBOR.encodeBytes (encodeByteString . Plutus.toBuiltin)

  genSomeNull :: Gen SomeValue
  genSomeNull = do
    return $ SomeValue () (const []) (const CBOR.encodeNull) (const encodeNull)

  genSomeList :: Int -> Gen SomeValue
  genSomeList n = do
    val <- genList (withMaxDepth (n - 1))
    let encodeCborg xs =
          mconcat
            [ CBOR.encodeListLen (fromIntegral (length xs))
            , foldMap (\(SomeValue x _ encode _) -> encode x) xs
            ]
    let encodeOurs =
          encodeList (\(SomeValue x _ _ encode) -> encode x)
    return $ SomeValue val (shrinkList shrinkSomeValue) encodeCborg encodeOurs

  genSomeListIndef :: Int -> Gen SomeValue
  genSomeListIndef n = do
    val <- genList (withMaxDepth (n - 1))
    let encodeCborg xs =
          mconcat
            [ CBOR.encodeListLenIndef
            , foldMap (\(SomeValue x _ encode _) -> encode x) xs
            , CBOR.encodeBreak
            ]
    let encodeOurs =
          encodeListIndef (\(SomeValue x _ _ encode) -> encode x)
    return $ SomeValue val (shrinkList shrinkSomeValue) encodeCborg encodeOurs

  genSomeMap :: Int -> Gen SomeValue
  genSomeMap n = do
    val <- genMap (withMaxDepth (n - 1)) (withMaxDepth (n - 1))
    let shrinkMap = shrinkList (liftShrink2 shrinkSomeValue shrinkSomeValue)
    let encodeCborg m =
          mconcat
            [ CBOR.encodeMapLen (fromIntegral $ length m)
            , foldMap
                ( \(SomeValue k _ encodeKey _, SomeValue v _ encodeValue _) ->
                    encodeKey k <> encodeValue v
                )
                m
            ]
    let encodeOurs =
          encodeMap
            (\(SomeValue k _ _ encodeKey) -> encodeKey k)
            (\(SomeValue v _ _ encodeValue) -> encodeValue v)
            . Plutus.Map.fromList
    return $ SomeValue val shrinkMap encodeCborg encodeOurs

  genSomeMapIndef :: Int -> Gen SomeValue
  genSomeMapIndef n = do
    val <- genMap (withMaxDepth (n - 1)) (withMaxDepth (n - 1))
    let shrinkMap = shrinkList (liftShrink2 shrinkSomeValue shrinkSomeValue)
    let encodeCborg m =
          mconcat
            [ CBOR.encodeMapLenIndef
            , foldMap
                ( \(SomeValue k _ encodeKey _, SomeValue v _ encodeValue _) ->
                    encodeKey k <> encodeValue v
                )
                m
            , CBOR.encodeBreak
            ]
    let encodeOurs =
          encodeMapIndef
            (\(SomeValue k _ _ encodeKey) -> encodeKey k)
            (\(SomeValue v _ _ encodeValue) -> encodeValue v)
            . Plutus.Map.fromList
    return $ SomeValue val shrinkMap encodeCborg encodeOurs

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

genByteString :: Gen ByteString
genByteString = do
  (\n -> BS.pack <$> vector n) =<< elements [0, 8, 16, 28, 32]

shrinkByteString :: ByteString -> [ByteString]
shrinkByteString =
  fmap BS.pack . shrink . BS.unpack

genList :: Gen a -> Gen [a]
genList genOne = do
  n <- elements [0, 1, 5, 25]
  vectorOf n genOne

genMap :: Gen k -> Gen v -> Gen [(k, v)]
genMap genKey genVal = do
  n <- elements [0, 1, 5, 25]
  zip <$> vectorOf n genKey <*> vectorOf n genVal
