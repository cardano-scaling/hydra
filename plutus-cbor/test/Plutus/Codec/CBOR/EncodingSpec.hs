{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Plutus.Codec.CBOR.EncodingSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
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
  encodeInteger,
  encodingToBuiltinByteString,
 )
import qualified PlutusTx as Plutus
import Test.Plutus.Codec.CBOR.Encoding.Validators (
  EncodeValidator,
  emptyValidator,
  encodeIntegerValidator,
 )
import Test.Plutus.Validator (
  evaluateScriptExecutionUnits,
 )
import Test.QuickCheck (
  Property,
  choose,
  conjoin,
  counterexample,
  forAll,
  forAllBlind,
  label,
  oneof,
  (===),
 )

spec :: Spec
spec = do
  describe "our CBOR encoding matches oracle's ('cborg' library)" $ do
    prop "for all (x :: Integer) w/ 'encodeInteger'" $
      forAll genInteger $
        propCompareWithOracle CBOR.encodeInteger encodeInteger

  describe "(on-chain) execution cost of CBOR encoding is small" $ do
    prop "for all (x :: Integer), <0.5%" $
      forAllBlind genInteger $
        propCostIsSmall
          (1 % 200)
          defaultMaxExecutionUnits
          (encodeInteger, encodeIntegerValidator)

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
  Plutus.ToData a =>
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
    & label ("of size = " <> show n <> ", mem units = " <> show mem <> ", CPU units = " <> show steps)
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
 where
  n = BS.length $ convert $ encode a
  ExUnits mem steps =
    distanceExUnits
      (evaluateScriptExecutionUnits emptyValidator ())
      (evaluateScriptExecutionUnits validator a)

  (relativeMemCost, relativeStepCost) =
    ( toInteger mem % toInteger maxMemUnits
    , toInteger steps % toInteger maxStepsUnits
    )

--
-- Compare scripts to baselines
--

-- | Current (2022-04-01) mainchain parameters.
defaultMaxExecutionUnits :: ExUnits
defaultMaxExecutionUnits =
  ExUnits
    { exUnitsMem = 10_000_000
    , exUnitsSteps = 10_000_000_000
    }

distanceExUnits :: ExUnits -> ExUnits -> ExUnits
distanceExUnits (ExUnits m0 s0) (ExUnits m1 s1) =
  ExUnits
    (if m0 > m1 then m0 - m1 else m1 - m0)
    (if s0 > s1 then s0 - s1 else s1 - s0)

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
