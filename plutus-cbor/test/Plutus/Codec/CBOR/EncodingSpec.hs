{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Plutus.Codec.CBOR.EncodingSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Pretty as CBOR
import qualified Codec.CBOR.Write as CBOR
import Data.Binary.Builder (toLazyByteString)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encodeBase16)
import Data.ByteString.Builder.Scientific (FPFormat (Fixed), formatScientificBuilder)
import Data.Ratio ((%))
import Data.Scientific (unsafeFromRational)
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Codec.CBOR.Encoding (
  Encoding,
  encodeByteString,
  encodeInteger,
  encodeList,
  encodingToBuiltinByteString,
 )
import qualified PlutusTx as Plutus
import Test.Plutus.Codec.CBOR.Encoding.Validators (
  EncodeValidator,
  emptyValidator,
  encodeByteStringValidator,
  encodeIntegerValidator,
  encodeListValidator,
 )
import Test.Plutus.Validator (
  ExUnits (..),
  defaultMaxExecutionUnits,
  distanceExUnits,
  evaluateScriptExecutionUnits,
 )
import Test.QuickCheck (
  Property,
  choose,
  conjoin,
  counterexample,
  elements,
  forAllBlind,
  forAllShrink,
  label,
  oneof,
  shrinkList,
  suchThat,
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

  describe "(on-chain) execution cost of CBOR encoding is small" $ do
    prop "for all small (< 65536) (x :: Integer), <0.15%" $
      forAllBlind (genInteger `suchThat` ((< 65536) . abs)) $
        propCostIsSmall
          (15 % 10_000)
          defaultMaxExecutionUnits
          (encodeInteger, encodeIntegerValidator)
    prop "for all large (> 65536) (x :: Integer), <0.30%" $
      forAllBlind (genInteger `suchThat` ((> 65536) . abs)) $
        propCostIsSmall
          (30 % 10_000)
          defaultMaxExecutionUnits
          (encodeInteger, encodeIntegerValidator)
    prop "for all (x :: ByteString), <0.05%" $
      forAllBlind genByteString $
        propCostIsSmall
          (5 % 10_000)
          defaultMaxExecutionUnits
          (encodeByteString, encodeByteStringValidator)
          . convert
    prop "for all (x :: [ByteString]), < (0.5% + 0.5% * n)" $
      forAllBlind (genList (convert <$> genByteString)) $ \xs ->
        let n = fromIntegral (length xs)
         in propCostIsSmall
              (50 % 10_000 + n * 50 % 10_000)
              defaultMaxExecutionUnits
              (encodeList . fmap encodeByteString, encodeListValidator)
              xs

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
  ( convert (CBOR.toStrictByteString oracle) === ours
  )
    & counterexample ("value: " <> show x)
    & counterexample ("\n─── cborg: \n" <> CBOR.prettyHexEnc oracle)
    & counterexample ("\n─── ours: \n" <> toString (encodeBase16 $ convert ours))
 where
  oracle = encodeOracle x
  ours = encodingToBuiltinByteString (encodeOurs x)

-- | Measure that the execution cost of encoding a certain value 'x' is small in
-- front of some max execution budget.
propCostIsSmall ::
  (Plutus.ToData a, Show a) =>
  Rational ->
  ExUnits ->
  (a -> Encoding, Scripts.TypedValidator (EncodeValidator a)) ->
  a ->
  Property
propCostIsSmall tolerance (ExUnits maxMemUnits maxStepsUnits) (encode, validator) a =
  conjoin
    [ relativeMemCost < tolerance
    , relativeStepCost < tolerance
    ]
    & label
      ( "of size = "
          <> show n
          <> ", mem units = "
          <> show mem
          <> " ("
          <> asPercent relativeMemCost
          <> " ), CPU units = "
          <> show steps
          <> " ("
          <> asPercent relativeStepCost
          <> " )"
      )
    & counterexample
      ( "memory execution units: "
          <> show mem
          <> " ("
          <> asPercent relativeMemCost
          <> ")"
      )
    & counterexample
      ( "CPU execution units:    "
          <> show steps
          <> " ("
          <> asPercent relativeStepCost
          <> ")"
      )
    & counterexample
      ("value:                   " <> show a)
 where
  n = BS.length $ convert $ encodingToBuiltinByteString $ encode a
  ExUnits mem steps =
    distanceExUnits
      (evaluateScriptExecutionUnits emptyValidator ())
      (evaluateScriptExecutionUnits validator a)

  (relativeMemCost, relativeStepCost) =
    ( toInteger mem % toInteger maxMemUnits
    , toInteger steps % toInteger maxStepsUnits
    )

--
-- Helpers
--

asPercent :: Rational -> String
asPercent r =
  decodeUtf8 (toLazyByteString $ toFixedDecimals 6 $ 100 * r) <> "%"
 where
  toFixedDecimals n = formatScientificBuilder Fixed (Just n) . unsafeFromRational

--
-- Generators
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

genSomeValue :: Gen SomeValue
genSomeValue =
  withMaxDepth 1
 where
  withMaxDepth :: Int -> Gen SomeValue
  withMaxDepth n =
    oneof $
      mconcat
        [
          [ do
              val <- genInteger
              return $ SomeValue val shrink CBOR.encodeInteger encodeInteger
          , do
              val <- genByteString
              return $ SomeValue val shrinkByteString CBOR.encodeBytes (encodeByteString . convert)
          ]
        , [ do
            val <- genList (withMaxDepth (n - 1))
            return $
              SomeValue
                val
                (shrinkList shrinkSomeValue)
                ( \xs ->
                    mconcat
                      [ CBOR.encodeListLen (fromIntegral (length xs))
                      , foldMap (\(SomeValue x _ encode _) -> encode x) xs
                      ]
                )
                (encodeList . fmap (\(SomeValue x _ _ encode) -> encode x))
          | n > 0
          ]
        ]

shrinkSomeValue :: SomeValue -> [SomeValue]
shrinkSomeValue (SomeValue a shrinker encode encode') =
  fmap (\a' -> SomeValue a' shrinker encode encode') (shrinker a)

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
  n <- elements [0, 8, 16, 28, 32]
  BS.pack <$> vector n

shrinkByteString :: ByteString -> [ByteString]
shrinkByteString =
  fmap BS.pack . shrink . BS.unpack

genList :: Gen a -> Gen [a]
genList genOne = do
  n <- elements [0, 1, 5, 25]
  vectorOf n genOne
