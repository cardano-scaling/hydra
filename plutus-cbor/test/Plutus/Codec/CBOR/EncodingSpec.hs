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
  encodingToBuiltinByteString,
 )
import qualified PlutusTx as Plutus
import Test.Plutus.Codec.CBOR.Encoding.Validators (
  EncodeValidator,
  emptyValidator,
  encodeByteStringValidator,
  encodeIntegerValidator,
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
  forAll,
  forAllBlind,
  label,
  oneof,
  suchThat,
  vector,
  (===),
 )

spec :: Spec
spec = do
  describe "our CBOR encoding matches oracle's ('cborg' library)" $ do
    prop "for all (x :: Integer) w/ 'encodeInteger'" $
      forAll genInteger $
        propCompareWithOracle CBOR.encodeInteger encodeInteger
    prop "for all (x :: ByteString) w/ 'encodeByteString'" $
      forAll genByteString $
        propCompareWithOracle CBOR.encodeBytes (encodeByteString . convert)

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
