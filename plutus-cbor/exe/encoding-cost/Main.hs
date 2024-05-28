import Hydra.Prelude hiding (label)

import Data.Binary.Builder (toLazyByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder.Scientific (FPFormat (Fixed), formatScientificBuilder)
import Data.Ratio ((%))
import Data.Scientific (unsafeFromRational)
import Plutus.Codec.CBOR.Encoding.Validator (
  ValidatorKind (..),
  encodeTxOutValidator,
  encodeTxOutsValidator,
 )
import PlutusLedgerApi.V1 qualified as Plutus
import PlutusTx.AssocMap qualified as Plutus.Map
import Test.Plutus.Validator (
  ExecutionUnits (..),
  defaultMaxExecutionUnits,
  evaluateScriptExecutionUnits,
 )
import Test.QuickCheck (
  choose,
  oneof,
  vector,
  vectorOf,
 )

main :: IO ()
main = do
  putTextLn "List of ADA-only TxOut, by list size."
  forM_ [1 .. 50] $ \n -> do
    let x = generateWith (vectorOf n genAdaOnlyTxOut) 42
    let (mem, cpu) = relativeCostOf x defaultMaxExecutionUnits encodeTxOutsValidator
    putTextLn @IO $
      unwords
        [ padLeft ' ' 2 (show n)
        , rationalToPercent mem
        , rationalToPercent cpu
        ]

  putTextLn ""
  putTextLn "Single multi-asset TxOut, by asset number."
  forM_ [1 .. 50] $ \n -> do
    let x = generateWith (genTxOut n) 42
    let (mem, cpu) = relativeCostOf x defaultMaxExecutionUnits encodeTxOutValidator
    putTextLn @IO $
      unwords
        [ padLeft ' ' 2 (show n)
        , rationalToPercent mem
        , rationalToPercent cpu
        ]

relativeCostOf ::
  Plutus.ToData a =>
  a ->
  ExecutionUnits ->
  (ValidatorKind -> Plutus.SerialisedScript) ->
  (Rational, Rational)
relativeCostOf a maxUnits mkValidator =
  (relativeMemCost, relativeCpuCost)
 where
  ExecutionUnits{executionMemory = maxMem, executionSteps = maxCpu} =
    maxUnits

  ExecutionUnits{executionMemory = mem, executionSteps = cpu} =
    either (error . show) id $ do
      base <- evaluateScriptExecutionUnits (mkValidator BaselineValidator) a
      real <- evaluateScriptExecutionUnits (mkValidator RealValidator) a
      pure $ distanceExecutionUnits base real

  (relativeMemCost, relativeCpuCost) =
    ( toInteger mem % toInteger maxMem
    , toInteger cpu % toInteger maxCpu
    )

distanceExecutionUnits :: ExecutionUnits -> ExecutionUnits -> ExecutionUnits
distanceExecutionUnits (ExecutionUnits c0 m0) (ExecutionUnits c1 m1) =
  ExecutionUnits
    (if c0 > c1 then c0 - c1 else c1 - c0)
    (if m0 > m1 then m0 - m1 else m1 - m0)

--
-- Helpers
--

rationalToPercent :: Rational -> Text
rationalToPercent r =
  padLeft ' ' 5 $ decodeUtf8 (toLazyByteString $ toFixedDecimals 2 $ 100 * r)
 where
  toFixedDecimals n = formatScientificBuilder Fixed (Just n) . unsafeFromRational

--
-- Generators
--

genTxOut :: Int -> Gen Plutus.TxOut
genTxOut n = do
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
  n <- genAssetQuantity
  policyId <- genCurrencySymbol
  assetName <- genTokenName
  pure $
    Plutus.Value $
      Plutus.Map.safeFromList
        [(policyId, Plutus.Map.safeFromList [(assetName, n)])]

genAdaOnlyValue :: Gen Plutus.Value
genAdaOnlyValue = do
  n <- genAssetQuantity
  pure $
    Plutus.Value $
      Plutus.Map.safeFromList
        [(Plutus.adaSymbol, Plutus.Map.safeFromList [(Plutus.adaToken, n)])]

genAssetQuantity :: Gen Integer
genAssetQuantity = choose (1, 4_294_967_296) -- NOTE: 2**32

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

genByteStringOf :: Int -> Gen ByteString
genByteStringOf n =
  BS.pack <$> vector n
