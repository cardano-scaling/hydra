{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Plutus.Codec.CBOR.EncodingSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Pretty as CBOR
import qualified Codec.CBOR.Write as CBOR
import Data.Binary.Builder (toLazyByteString)
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
  encodeListIndef,
  encodeMap,
  encodeMapIndef,
  encodeNull,
  encodingToBuiltinByteString,
 )
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified PlutusTx.AssocMap as Plutus.Map
import Test.Plutus.Codec.CBOR.Encoding.Validators (
  EncodeValidator,
  ValidatorKind (..),
  encodeByteStringValidator,
  encodeIntegerValidator,
  encodeListValidator,
  encodeMapValidator,
  encodeTxOutValidator,
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
  liftShrink2,
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
    prop "for all small (< 65536) (x :: Integer)" $
      forAllBlind (genInteger `suchThat` ((< 65536) . abs)) $
        propCostIsSmallerThan
          (0.20, 0.10)
          defaultMaxExecutionUnits
          encodeIntegerValidator
    prop "for all large (> 65536) (x :: Integer)" $
      forAllBlind (genInteger `suchThat` ((> 65536) . abs)) $
        propCostIsSmallerThan
          (0.40, 0.20)
          defaultMaxExecutionUnits
          encodeIntegerValidator
    prop "for all (x :: ByteString)" $
      forAllBlind genByteString $
        propCostIsSmallerThan
          (0.20, 0.1)
          defaultMaxExecutionUnits
          encodeByteStringValidator
          . Plutus.toBuiltin
    prop "for all (x :: [ByteString])" $
      forAllBlind (genList (Plutus.toBuiltin <$> genByteString)) $ \xs ->
        let n = fromIntegral (length xs)
         in propCostIsSmallerThan
              (0.4 * n + 0.5, n * 0.2 + 0.5)
              defaultMaxExecutionUnits
              encodeListValidator
              xs
              & label ("of length = " <> show n)
              & counterexample ("length: " <> show n)
    prop "for all (x :: Map ByteString ByteString)" $
      forAllBlind (genMap (Plutus.toBuiltin <$> genByteString) (Plutus.toBuiltin <$> genByteString)) $ \m ->
        let n = fromIntegral (length m)
         in propCostIsSmallerThan
              (n * 0.5 + 0.5, n * 0.3 + 0.5)
              defaultMaxExecutionUnits
              encodeMapValidator
              (Plutus.Map.fromList m)
              & label ("of size = " <> show n)
              & counterexample ("size: " <> show n)
    prop "for all (x :: TxOut), ada-only" $
      forAllBlind genAdaOnlyTxOut $
        propCostIsSmallerThan
          (2.5, 1.5)
          defaultMaxExecutionUnits
          encodeTxOutValidator
    prop "for all (x :: TxOut), with up to 10 assets" $
      forAllBlind (genTxOut 10) $
        propCostIsSmallerThan
          (15, 6)
          defaultMaxExecutionUnits
          encodeTxOutValidator
    prop "for all (x :: TxOut), with up to 75 assets" $
      forAllBlind (genTxOut 75) $
        propCostIsSmallerThan
          (100, 40)
          defaultMaxExecutionUnits
          encodeTxOutValidator

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

-- | Measure that the execution cost of encoding a certain value 'x' is small in
-- front of some max execution budget.
propCostIsSmallerThan ::
  (Plutus.ToData a, Show a) =>
  (Double, Double) ->
  ExUnits ->
  (ValidatorKind -> Scripts.TypedValidator (EncodeValidator a)) ->
  a ->
  Property
propCostIsSmallerThan (upperBoundMem, upperBoundStep) (ExUnits maxMemUnits maxStepsUnits) validator a =
  conjoin
    [ 100 * fromRational relativeMemCost < upperBoundMem
        & counterexample
          ( "memory execution units exceeded: "
              <> show mem
              <> " ("
              <> asPercent relativeMemCost
              <> ")"
          )
    , 100 * fromRational relativeStepCost < upperBoundStep
        & counterexample
          ( "CPU execution units exceeded:    "
              <> show steps
              <> " ("
              <> asPercent relativeStepCost
              <> ")"
          )
    ]
    & label
      ( "of mem units < "
          <> show upperBoundMem
          <> "%, and steps units < "
          <> show upperBoundStep
          <> "%"
      )
    & counterexample
      ("value:                   " <> show a)
 where
  ExUnits mem steps =
    distanceExUnits
      (evaluateScriptExecutionUnits (validator BaselineValidator) a)
      (evaluateScriptExecutionUnits (validator RealValidator) a)

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
  genByteStringOf =<< elements [0, 8, 16, 28, 32]

genByteStringOf :: Int -> Gen ByteString
genByteStringOf n =
  BS.pack <$> vector n

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

genTxOut :: Int -> Gen Plutus.TxOut
genTxOut high = do
  n <- choose (1, high)
  Plutus.TxOut
    <$> genAddress
    <*> fmap mconcat (vectorOf n genValue)
    <*> oneof [pure Nothing, Just <$> genDatumHash]

genAdaOnlyTxOut :: Gen Plutus.TxOut
genAdaOnlyTxOut =
  Plutus.TxOut
    <$> genAddress
    <*> genAdaOnlyValue
    <*> oneof [pure Nothing, Just <$> genDatumHash]

genAddress :: Gen Plutus.Address
genAddress =
  Plutus.Address
    <$> fmap (Plutus.PubKeyCredential . Plutus.PubKeyHash . Plutus.toBuiltin) (genByteStringOf 28)
    <*> pure Nothing

genValue :: Gen Plutus.Value
genValue = do
  n <- genInteger `suchThat` (> 0)
  policyId <- genCurrencySymbol
  assetName <- genTokenName
  pure $
    Plutus.Value $
      Plutus.Map.fromList
        [(policyId, Plutus.Map.fromList [(assetName, n)])]

genAdaOnlyValue :: Gen Plutus.Value
genAdaOnlyValue = do
  n <- genInteger `suchThat` (> 0)
  pure $
    Plutus.Value $
      Plutus.Map.fromList
        [(Plutus.adaSymbol, Plutus.Map.fromList [(Plutus.adaToken, n)])]

genCurrencySymbol :: Gen Plutus.CurrencySymbol
genCurrencySymbol =
  Plutus.CurrencySymbol
    <$> fmap Plutus.toBuiltin (genByteStringOf 32)

genTokenName :: Gen Plutus.TokenName
genTokenName =
  Plutus.TokenName
    <$> fmap Plutus.toBuiltin (genByteStringOf =<< choose (0, 32))

genDatumHash :: Gen Plutus.DatumHash
genDatumHash =
  Plutus.DatumHash
    <$> fmap Plutus.toBuiltin (genByteStringOf 32)
